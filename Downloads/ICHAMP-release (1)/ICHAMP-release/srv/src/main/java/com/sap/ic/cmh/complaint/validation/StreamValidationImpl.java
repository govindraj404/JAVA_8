package com.sap.ic.cmh.complaint.validation;

import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Streams;
import cds.gen.complaintservice.Streams_;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StreamValidationImpl implements StreamValidation {

    @Autowired
    MasterDataService masterDataService;
    @Autowired
    ComplaintService complaintService;

    @Autowired
    StreamService streamServie;
    @Autowired
    Messages messages;

    public static final Logger logger = LoggerHelper.getLogger(StreamValidationImpl.class);
	private static final String STREAM_VALIDATION_IMPL = "StreamValidationImpl";



    /**
     * Validate Adapt stream
     * 
     * @param streams
     */
    @Override
    public void validateStreamEdit(Streams streams) {
    	LoggerHelper.logMethodEntry(logger, STREAM_VALIDATION_IMPL, "validateStreamEdit");
        Streams stream = streamServie.getStream(streams.getId()).single(Streams.class);


        if (Boolean.TRUE.equals(isComplaintStatusValid(stream.getParentIDId()))) {
            // case 3: if Stream has only one BO and that BO is already created
            validateStreamFlagging(stream, streams.getIsRelevant());
            logger.info("Stream Flagged and inside checking the relevant stream status");
        } else {
            messages.error(MessageKeys.INVALID_COMPLAINT_STATE).target("in", Streams_.class,
                    streamsParent -> stream.getParentIDId());
        }
        LoggerHelper.logMethodExit(logger, STREAM_VALIDATION_IMPL, "validateStreamEdit");
    }


    /**
     * Check for valid Complaint Status
     * 
     * @param parentIDId
     * @return Boolean
     */
    private Boolean isComplaintStatusValid(String parentIDId) {
    	LoggerHelper.logMethodEntry(logger, STREAM_VALIDATION_IMPL, "isComplaintStatusValid");
        Complaints complaint = complaintService.getComplaintDetails(parentIDId);
        LoggerHelper.logMethodExit(logger, STREAM_VALIDATION_IMPL, "isComplaintStatusValid");
        return ((complaint.getComplaintStatusCode() != null)
                && (!complaint.getComplaintStatusCode().equals(Constants.COMPLAINT_CLOSED)
                        || !complaint.getComplaintStatusCode().equals(Constants.STATUS_NEW) || !complaint.getComplaintStatusCode().equals(Constants.COMPLAINT_DISCARDED)));
    }


    /**
     * Validate BusinessObjectFlagging
     * 
     * @param businessObjects
     */
    private void validateBusinessObjectFlagging(BusinessObjects businessObjects,
            Boolean isRelevant) {
    	LoggerHelper.logMethodEntry(logger, STREAM_VALIDATION_IMPL, "validateBusinessObjectFlagging");
        if (isRelevant != null && Boolean.FALSE.equals(isRelevant)) {
            if (businessObjects.getBusinessObjectTypeCode()
                    .equals(Constants.QUALITYNOTIFICATION_CODE)) {
                messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT).target("in", Streams_.class,
                        businessObject -> businessObjects.getBusinessObjectTypeCode());

            } else if (businessObjects.getBusinessObjectIDId() != null) {

                messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED).target("in", Streams_.class,
                        businessObject -> businessObjects.getBusinessObjectTypeCode());
            }
        }
        LoggerHelper.logMethodExit(logger, STREAM_VALIDATION_IMPL, "validateBusinessObjectFlagging");
    }


    /**
     * Validate Stream Flagging
     * 
     * @param stream
     */
    private void validateStreamFlagging(Streams stream, Boolean isRelevant) {
    	LoggerHelper.logMethodEntry(logger, STREAM_VALIDATION_IMPL, "validateStreamFlagging");
        if (isRelevant != null && !isRelevant) {
            if (stream.getStreamTypeCode().equals(Constants.QUALITY_STREAM_CODE)) {
                messages.error(MessageKeys.MANDATORY_STREAM).target("in", Streams_.class,
                        bo -> stream.getStreamTypeCode());
            }

            else if (stream.getBusinessObjects() != null
                    && stream.getBusinessObjects().size() == 1) {
                BusinessObjects businessObjects = stream.getBusinessObjects().get(0);

                if (businessObjects != null && checkBusinessObjectsCondition(businessObjects)) {
                    messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED).target("in", Streams_.class,
                            bo -> stream.getStreamTypeCode());
                }
            }
        }
        LoggerHelper.logMethodExit(logger, STREAM_VALIDATION_IMPL, "validateStreamFlagging");
    }



    /**
     * Check if BusinessObject is relavant
     * 
     * @param businessObjects
     * @return boolean
     */
    private boolean checkBusinessObjectsCondition(BusinessObjects businessObjects) {
        return (businessObjects.getBusinessObjectIDId() != null);
    }


    /**
     * Validate BusinessObjects flagging for adapt streams
     * 
     * @param eachBo
     */
    @Override
    public void validateBusinessObjectsEdit(BusinessObjects eachBo) {
    	LoggerHelper.logMethodEntry(logger, STREAM_VALIDATION_IMPL, "validateBusinessObjectsEdit");
        BusinessObjects eachBusinessObject =
                streamServie.getBusinessObject(eachBo.getId()).single(BusinessObjects.class);
        if (Boolean.TRUE.equals(isComplaintStatusValid(eachBusinessObject.getComplaint()))) {
            // case 2: if Bo is already created and is mandatoty
            validateBusinessObjectFlagging(eachBusinessObject, eachBo.getIsRelevant());
        }
        LoggerHelper.logMethodExit(logger, STREAM_VALIDATION_IMPL, "validateBusinessObjectsEdit");
    }

}
