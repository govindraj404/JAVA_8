package com.sap.ic.cmh.complaint.service;

import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes;
import cds.gen.com.sap.ic.cmh.claimprocessingstreamstatus.ClaimProcessingStreamStatuses;
import cds.gen.com.sap.ic.cmh.complainttypestreamtypemapping.ComplaintTypeStreamTypeMappings;
import cds.gen.com.sap.ic.cmh.qualitystreamstatus.QualityStreamStatuses;
import cds.gen.com.sap.ic.cmh.streamtype.StreamTypes;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Streams;
import cds.gen.returnpurchaseorderservice.LogisticsStreamStatuses;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.ErrorStatuses;
import com.sap.cds.services.ServiceException;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.persistency.StreamDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Service
public class StreamServiceImpl implements StreamService {

    @Autowired
    BusinessObjectService businessObjectService;
    @Autowired
    StreamDao streamDao;
    @Autowired
    ComplaintService complaintService;

    private static final String STREAM_SERVICE_IMPL = "StreamServiceImpl";
    private static final Logger logger = LoggerFactory.getLogger(StreamServiceImpl.class);
    private static final String GET_CURRENT_BO_STATUS = "getCurrentBOStatus";

    /**
     * Create all the Streams once the Complaint gets Created
     * 
     * @param complaintId
     */
    @Override
    public void createAllStreams(String complaintId, String complaintType) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, "createAllStreams");
        Result streamType = streamDao.getStreamType();
        List<StreamTypes> types = streamType.listOf(StreamTypes.class);
        Result run = streamDao.getBusinessObjectTypes();
        List<BusinessObjectTypes> boTypes = run.listOf(BusinessObjectTypes.class);
        HashMap<String, String> map = new HashMap<>();
        try {
            for (StreamTypes stream : types) {
                Result sequenceResult = streamDao.getSequenceBasedOnComplaintStreamType(stream.getCode(),
                        complaintType);
                int sequenceNumber = sequenceResult.first().isPresent() ? Integer.valueOf(
                        sequenceResult.list().get(0).get(ComplaintTypeStreamTypeMappings.SEQUENCE_NUMBER).toString())
                        : 0;
                Streams streams = Struct.create(Streams.class);
                streams.setIsRelevant(true);
                streams.setStreamTypeCode(stream.getCode());
                streams.setParentIDId(complaintId);
                if (sequenceNumber > 0) {
                    streams.setSequenceNumber(sequenceNumber);
                }
                Result streamResult = streamDao.createStream(streams);
                Streams fetchStream = streamResult.single(Streams.class);
                map.put(stream.getCode(), fetchStream.getId());
                logger.info("Streams Created successfully");
            }
        } catch (ODataException e) {
            logger.error("Issue while creating  Streams");
            return;
        }
        try {
            for (BusinessObjectTypes businessObjectTypes : boTypes) {
                BusinessObjects bo = Struct.create(BusinessObjects.class);
                bo.setStreamId(map.get(businessObjectTypes.getStreamTypeCode()));
                bo.setBusinessObjectTypeCode(businessObjectTypes.getCode());
                bo.setIsRelevant(true);
                bo.setComplaint(complaintId);
                streamDao.insertBusinessObjects(bo);
                logger.info("Business Objects Created successfully");
            }
        } catch (ODataException e) {
            logger.error("Issue while creating  BusinessObjects");
        }
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, "createAllStreams");
    }

    /**
     * Check if BusinessObject has already performed with the respective actions (
     * hence the status has been updated)
     * 
     * @param complaint
     * @param boType
     * @param boStatus
     * @return Result
     */
    @Override
    public Result getBusinessObjectsWithStatus(String complaint, String boType, String boStatus) {
        return streamDao.getBusinessObjectsWithStatus(complaint, boType, boStatus);

    }

    /**
     * get BusinessObject based on ID
     * 
     * @param id
     * @return Result
     */
    @Override
    public Result getBusinessObject(String businessObjectID) {
        return streamDao.getBusinessObject(businessObjectID);
    }

    /**
     * get Stream based on ID
     * 
     * @param id
     * @return Result
     */
    @Override
    public Result getStream(String id) {
        return streamDao.getStream(id);
    }

    /**
     * Invoke this from respective handlers to Update Stream status
     */
    @Override
    public void updateStreamStatus(String businessObjectId, String boType, boolean isActive) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, "updateStreamStatus");
        getBusinessObjectsBasedOnBusinessObjectId(businessObjectId, boType, isActive);
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, "updateStreamStatus");

    }

    /**
     * Get the BusinessObject ID for a BO type and complaint id
     */
    public void getBusinessObjectsBasedOnBusinessObjectId(String businessObjectId, String boType, boolean isActive) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, "getBusinessObjectsBasedOnBusinessObjectId");
        logger.info("BOType is received.");
        BusinessObjects businessObjectsBasedOnBusinessObjectId = businessObjectService
                .getBusinessObjectsBasedOnBusinessObjectId(businessObjectId);
        if (null != businessObjectsBasedOnBusinessObjectId) {
            String complaintId = businessObjectsBasedOnBusinessObjectId.getComplaint();
            String streamId = businessObjectsBasedOnBusinessObjectId.getStreamId();

            getCurrentBOStatus(complaintId, boType, streamId, businessObjectId, isActive);
        } else {
            logger.error("There is no BusinessObject associated with the given complaint and BOType");
            throw new ServiceException(ErrorStatuses.SERVER_ERROR,
                    MessageKeys.BUSINESS_OBJECT_NOT_ASSOCIATED_WITH_COMPLAINT);

        }
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, "getBusinessObjectsBasedOnBusinessObjectId");
    }

    /**
     * Get current Business Object status of a particular BO based on its ID
     * 
     * @param businessObjectId
     * @param isActive
     */
    @Override
    public void getCurrentBOStatus(String complaintId, String boType, String streamId, String businessObjectId,
            boolean isActive) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
        Result run;
        String currentBOStatus = determineCurrentBOStatus(boType, businessObjectId, isActive);
        logger.info("Current BO Status is : {} ", currentBOStatus);
        String streamStatusCode = "";
        switch (boType) {

            case Constants.CLAIM_CODE:
                run = streamDao.findClaimStreamStatus(currentBOStatus);
                streamStatusCode = run.first().isPresent()
                        ? run.list().get(0).get(ClaimProcessingStreamStatuses.STREAM_STATUS_CODE).toString()
                        : "";
                logger.info("Claim's corresponding cockpit status: ", streamStatusCode);
                break;
            case Constants.QUALITYNOTIFICATION_CODE:
            case Constants.SUPPLIER_EIGHTD_CODE:
                streamStatusCode = determineQualityStreamStatus(complaintId, boType, currentBOStatus);

                logger.info("Quality Notification's corresponding cockpit status: ", streamStatusCode);
                break;
            case Constants.RETURNPO_CODE:
                run = streamDao.findLogisticsStreamStatus(currentBOStatus);
                streamStatusCode = run.first().isPresent()
                        ? run.list().get(0).get(LogisticsStreamStatuses.STREAM_STATUS_CODE).toString()
                        : "";
                logger.info("Return PO's corresponding cockpit status: ", streamStatusCode);
                break;
            default:
                break;
        }
        setStreamStatusAndUpdateStreams(streamId, streamStatusCode);
        updateComplaintStatusBasedOnAllStreamStatus(complaintId);
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
    }

    private String determineQualityStreamStatus(String complaintId, String boType, String currentBOStatus) {
        String status = "";
        Map<String, Object> boMap = new HashMap<>();
        Result run;
        String streamStatusCode;
        status = boType.equalsIgnoreCase(Constants.QUALITYNOTIFICATION_CODE)
                ? businessObjectService.getBusinessObjectStatusBasedOnType(complaintId, Constants.SUPPLIER_EIGHTD_CODE)
                : businessObjectService.getBusinessObjectStatusBasedOnType(complaintId,
                        Constants.QUALITYNOTIFICATION_CODE);
        String key = boType.equalsIgnoreCase(Constants.QUALITYNOTIFICATION_CODE) ? Constants.SUPPLIER_EIGHTD_CODE
                : Constants.QUALITYNOTIFICATION_CODE;
        boolean isEightDRelevant = businessObjectService.checkIfBOIsRelevant(complaintId,
                Constants.SUPPLIER_EIGHTD_CODE);
        boMap.put(boType, currentBOStatus);
        boMap.put(key, status);
        boMap.put(Constants.IS_EIGHTD_RELEVANT, isEightDRelevant);
        run = streamDao.findQualityStreamStatus(currentBOStatus, boMap);
        streamStatusCode = run.first().isPresent()
                ? run.list().get(0).get(QualityStreamStatuses.STREAM_STATUS_CODE).toString()
                : "";
        return streamStatusCode;
    }

    /**
     * Determine the current BO status
     * 
     * @param boType
     * @param businessObjectId
     * @param isActive
     * @return
     */
    public String determineCurrentBOStatus(String boType, String businessObjectId, boolean isActive) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
        String currentBOStatus;
        logger.info("businessObjectId : {} ", businessObjectId);
        logger.info("isActive : {} ", isActive);
        if (boType.equalsIgnoreCase(Constants.SUPPLIER_EIGHTD_CODE)
                || boType.equalsIgnoreCase(Constants.QUALITYNOTIFICATION_CODE)) {
            currentBOStatus = isActive ? businessObjectService.getCurrentActiveBOStatusSupplier8D(businessObjectId)
                    : businessObjectService.getCurrentBOStatusSupplier8D(businessObjectId);
        } else {

            currentBOStatus = isActive ? businessObjectService.getCurrentActiveBOStatus(businessObjectId)
                    : businessObjectService.getCurrentBOStatus(businessObjectId);
        }
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
        return currentBOStatus;
    }

    /**
     * Once Stream status is retrieved, set the stream status and update the stream
     * entity
     *
     * @param streamId
     * @param streamStatusCode
     */
    public void setStreamStatusAndUpdateStreams(String streamId, String streamStatusCode) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
        Streams streams = Struct.create(Streams.class);
        streams.setStatusCode(streamStatusCode);
        streamDao.updateStreams(streamId, streams);
        logger.info("Streams updated successfully! {} ", streamStatusCode);
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
    }

    /**
     * Check the status for all relevant streams If all are closed, update the
     * complaint status
     * 
     * @param complaintId
     */
    @Override
    public void updateComplaintStatusBasedOnAllStreamStatus(String complaintId) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
        Complaints complaints = complaintService.getComplaintDetails(complaintId);
        String complaintStatusCode = complaints.getComplaintStatusCode();
        logger.info("[updateComplaintStatusBasedOnAllStreamStatus] complaintStatusCode : {} ", complaintStatusCode);
        if (checkIfAllRelevantStreamsClosed(complaintId)) {
            logger.info("All Relevant streams are closed ");
            if (!Constants.COMPLAINT_REVISED.equalsIgnoreCase(complaintStatusCode)) {
                complaintService.updateComplaintStatus(complaintId, Constants.COMPLAINT_CLOSED);
            }
        } else if (Constants.COMPLAINT_IN_PROGRESS.equalsIgnoreCase(complaintStatusCode)||
        Constants.COMPLAINT_CLOSED.equalsIgnoreCase(complaintStatusCode)) {
            logger.info("Complaint status as INPR ");
            complaintService.updateComplaintStatus(complaintId, Constants.COMPLAINT_IN_PROGRESS);
        }
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, GET_CURRENT_BO_STATUS);
    }

    /**
     * Check the status for all relevant streams If all are closed
     *
     * @param complaintId
     */
    @Override
    public boolean checkIfAllRelevantStreamsClosed(String complaintId) {
        LoggerHelper.logMethodEntry(logger, STREAM_SERVICE_IMPL, "checkIfAllRelevantStreamsClosed");
        boolean checkIfAllRelevantStreamsClosed = false;
        logger.info("Check Stream status for Complaint ID : {} ", complaintId);
        Result checkAllRelevantStreamStatusResult = streamDao.checkAllStreamStatus(complaintId);
        List<Streams> relevantStreamList = checkAllRelevantStreamStatusResult.first().isPresent()
                ? checkAllRelevantStreamStatusResult.listOf(Streams.class)
                : null;

        if (null != relevantStreamList && !CollectionUtils.isEmpty(relevantStreamList)) {
            logger.info("relevantStreamList size : {} ", relevantStreamList.size());
            checkIfAllRelevantStreamsClosed = determineStreamStatusAndMarkClosed(checkIfAllRelevantStreamsClosed, relevantStreamList);
            checkIfAllRelevantStreamsClosed = relevantStreamList.stream()
            .allMatch(st -> StringUtils.isNotBlank(st.getStatusCode())
                    && st.getStatusCode().equalsIgnoreCase(Constants.COMPLAINT_CLOSED));
        }
        LoggerHelper.logMethodExit(logger, STREAM_SERVICE_IMPL, "checkIfAllRelevantStreamsClosed");
        return checkIfAllRelevantStreamsClosed;
    }

    public boolean determineStreamStatusAndMarkClosed(boolean checkIfAllRelevantStreamsClosed, List<Streams> relevantStreamList) {
            logger.info("determineStreamStatusAndMarkClosed .... {} ", checkIfAllRelevantStreamsClosed);
            Optional<Streams> findAny = relevantStreamList.stream()
                    .filter(st -> st.getStreamTypeCode().equalsIgnoreCase(Constants.QUALITY_STREAM_CODE)).findAny();
            Streams streams = findAny.isPresent() ? findAny.get() : null;
            // If only Quality stream is relevant, check the stream status and make it
            // closed
            if (null != streams) {
                logger.info("Stream Type that is relevant  {} :: ", streams.getStreamTypeCode());
                logger.info("Stream Type Status that is relevant  {} :: ", streams.getStatusCode());
                String updateQualityStreamStatus = updateQualityStreamStatus(streams);
                if (streams.getStreamTypeCode().equalsIgnoreCase(Constants.QUALITY_STREAM_CODE)
                && StringUtils.isNotBlank(updateQualityStreamStatus)) {        
                        streams.setStatusCode(updateQualityStreamStatus);
                        streamDao.updateStreams(streams.getId(), streams);
                        checkIfAllRelevantStreamsClosed = relevantStreamList.size() == 1
                    && relevantStreamList.stream().anyMatch(st -> StringUtils.isNotBlank(st.getStreamTypeCode())
                    && st.getStreamTypeCode().equalsIgnoreCase(Constants.QUALITY_STREAM_CODE));
                }
                
            }
        logger.info("checkIfAllRelevantStreamsClosed .... {} ",checkIfAllRelevantStreamsClosed);
        return checkIfAllRelevantStreamsClosed;
    }

    /**
     * If QN is closed and 8D marked as irrelevant , close QLTY stream
     */
    @Override
    public String updateQualityStreamStatus(Streams stream) {
        logger.info("[On Update Streams] Inside updating Quality Stream status");
        String qualityStreamStatus = "";
        String businessObjectId = businessObjectService
                .getBusinessObjectIdBasedOnTypeAndComplaint(stream.getParentIDId(), Constants.QUALITYNOTIFICATION_CODE);
        logger.info("QN businessObjectId :: {} ", businessObjectId);
        if (StringUtils.isNotBlank(businessObjectId)) {
            String currentBOStatus = determineCurrentBOStatus(Constants.QUALITYNOTIFICATION_CODE, businessObjectId,
                    false);
            qualityStreamStatus = determineQualityStreamStatus(stream.getParentIDId(),
                    Constants.QUALITYNOTIFICATION_CODE, currentBOStatus);
            logger.info("QN qualityStreamStatus :: {} ", qualityStreamStatus);
        }
        return qualityStreamStatus;
    }
}
