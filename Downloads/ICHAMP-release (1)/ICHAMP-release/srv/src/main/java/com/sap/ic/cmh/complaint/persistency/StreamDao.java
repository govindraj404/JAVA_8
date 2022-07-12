package com.sap.ic.cmh.complaint.persistency;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.apache.commons.lang3.StringUtils;

import com.sap.cds.Result;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes_;
import cds.gen.com.sap.ic.cmh.claimprocessingstreamstatus.ClaimProcessingStreamStatuses;
import cds.gen.com.sap.ic.cmh.claimprocessingstreamstatus.ClaimProcessingStreamStatuses_;
import cds.gen.com.sap.ic.cmh.complainttypestreamtypemapping.ComplaintTypeStreamTypeMappings_;
import cds.gen.returnpurchaseorderservice.LogisticsStreamStatuses;
import cds.gen.returnpurchaseorderservice.LogisticsStreamStatuses_;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatus.QualityNotificationStatuses;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatus.QualityNotificationStatuses_;
import cds.gen.com.sap.ic.cmh.qualitystreamstatus.QualityStreamStatuses;
import cds.gen.com.sap.ic.cmh.qualitystreamstatus.QualityStreamStatuses_;
import cds.gen.com.sap.ic.cmh.streamtype.StreamTypes_;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatus.SupplierIssueProcessStatuses;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatus.SupplierIssueProcessStatuses_;
import cds.gen.complaintservice.BusinessObjectMappings;
import cds.gen.complaintservice.BusinessObjectMappings_;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.BusinessObjects_;
import cds.gen.complaintservice.Streams;
import cds.gen.complaintservice.Streams_;



@Repository
public class StreamDao {

    @Autowired
    PersistenceService db;
    
    private static final String STREAM_DAO = "StreamDao";
	private static final Logger logger = LoggerFactory.getLogger(StreamDao.class);

    /**
     * Get the stream level status of claim based on its current BO level status
     * 
     * @param currentBOStatus
     * @return
     */
    public Result findClaimStreamStatus(String currentBOStatus) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "findClaimStreamStatus");
        CqnSelect select = Select.from(ClaimProcessingStreamStatuses_.class)
                .columns(ClaimProcessingStreamStatuses.STREAM_STATUS_CODE)
                .where(clm -> clm.claimStatus_code().eq(currentBOStatus));
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "findClaimStreamStatus");
       return db.run(select);
        
        
    }

    /**
     * Get the stream level status of Quality based on its current BO level status
     * 
     * @param currentBOStatus
     * @param businessObjectId
     * @return
     */
    public Result findQualityStreamStatus(String currentBOStatus,
            Map<String, Object> qnSupplierIssueProcessStatusMap) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "findQualityStreamStatus");
        Result qualityStreamStatus = null;
        // get the sequence based on code from status entity
        // seq numbr and rowcount in status -> match it .. if same, get the 8D status
        // also
        String qualityNotificationStatusSequenceNumber =
                getQualityNotificationStatusSequenceNumber(currentBOStatus);
        String qualityNotificationStatusRowCount = getAllQualityNotificationStatuses();
        // If record is there in QN status, get the row count and check if its matching
        // with sequence
        if (!qualityNotificationStatusSequenceNumber.equals("0")) {
            // If QN row count and seq number are matching, get the 8D count as well
            if (qualityNotificationStatusSequenceNumber.equals(qualityNotificationStatusRowCount)) {
                String supplier8DSequenceNumber =
                        null != qnSupplierIssueProcessStatusMap.get(Constants.SUPPLIER_EIGHTD_CODE).toString()
                                ? getSupplier8DStatusSequence(qnSupplierIssueProcessStatusMap.get(Constants.SUPPLIER_EIGHTD_CODE).toString())
                                : "0";
                String supplier8DStatusRowCount = getAllSupplier8DStatusesRowCount();
                if (supplier8DSequenceNumber.equals(supplier8DStatusRowCount)) {
                    qualityStreamStatus =
                            getQualityStreamStatusQNEightD(qnSupplierIssueProcessStatusMap);
                } else {
                	
                    qualityStreamStatus = getQualityStreamStatusQNOrEightD(qnSupplierIssueProcessStatusMap);
                }
            } else {
            	
                qualityStreamStatus = getQualityStreamStatusQNOrEightD(qnSupplierIssueProcessStatusMap);
            }
            // If record is not there in QN, check in 8D
        } else {
            qualityStreamStatus = determineStreamStatusWithEightD(currentBOStatus,
                    qnSupplierIssueProcessStatusMap);
        }
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "findQualityStreamStatus");
        return qualityStreamStatus;
    }

    /**
     * Check 8D status and then Quality notification status
     * @param currentBOStatus
     * @param qnSupplierIssueProcessStatusMap
     * @return
     */
    private Result determineStreamStatusWithEightD(String currentBOStatus,
            Map<String, Object> qnSupplierIssueProcessStatusMap) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "determineStreamStatusWithEightD");
        Result qualityStreamStatus;
        String supplier8DSequenceNumber = getSupplier8DStatusSequence(currentBOStatus);
        String supplier8DStatusRowCount = getAllSupplier8DStatusesRowCount();
        // If 8D row count and seq number are matching, get the QN count as well
        if (supplier8DSequenceNumber.equals(supplier8DStatusRowCount)) {
            String qualityNotificationStatusSequenceNumber1 =
                    null != qnSupplierIssueProcessStatusMap.get(Constants.QUALITYNOTIFICATION_CODE).toString()
                            ? getQualityNotificationStatusSequenceNumber(qnSupplierIssueProcessStatusMap.get(Constants.QUALITYNOTIFICATION_CODE).toString())
                            : "0";
            String qualityNotificationStatusRowCount1 = getAllQualityNotificationStatuses();
            if (qualityNotificationStatusSequenceNumber1
                    .equals(qualityNotificationStatusRowCount1)) {
                qualityStreamStatus =
                        getQualityStreamStatusQNEightD(qnSupplierIssueProcessStatusMap);
            } else {
                qualityStreamStatus = getQualityStreamStatusQNOrEightD(qnSupplierIssueProcessStatusMap);
            }
        } else {
            qualityStreamStatus = getQualityStreamStatusQNOrEightD(qnSupplierIssueProcessStatusMap);
        }
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "determineStreamStatusWithEightD");
        return qualityStreamStatus;
    }

    /**
     * Total row count for QN
     * 
     * @return
     */
    public String getAllQualityNotificationStatuses() {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getAllQualityNotificationStatuses");
        CqnSelect selectAllQn = Select.from(QualityNotificationStatuses_.class);
        Result runAll = db.run(selectAllQn);
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "getAllQualityNotificationStatuses");
        return runAll.first().isPresent() ? String.valueOf(runAll.list().size()) : "0";
    }

    /**
     * Sequence number for the BO status of QN
     * 
     * @param currentBOStatus
     * @return
     */
    public String getQualityNotificationStatusSequenceNumber(String currentBOStatus) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getQualityNotificationStatusSequenceNumber");
        CqnSelect selectSequenceNumberQn = Select.from(QualityNotificationStatuses_.class)
                .columns(QualityNotificationStatuses.SEQUENCE_NUMBER)
                .where(qns -> qns.code().eq(currentBOStatus));
        Result runSequence = db.run(selectSequenceNumberQn);
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "getQualityNotificationStatusSequenceNumber");
        return runSequence.first().isPresent()
                ? runSequence.list().get(0).get(QualityNotificationStatuses.SEQUENCE_NUMBER)
                        .toString()
                : "0";
    }

    /**
     * Sequence number for the BO status of 8D
     * 
     * @param currentBOStatus
     * @return
     */
    public String getSupplier8DStatusSequence(String currentBOStatus) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getSupplier8DSequence");
        CqnSelect selectSequenceNumber8D = Select.from(SupplierIssueProcessStatuses_.class)
                .columns(SupplierIssueProcessStatuses.SEQUENCE_NUMBER)
                .where(qns -> qns.code().eq(currentBOStatus));
        Result runSequence8D = db.run(selectSequenceNumber8D);
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "getSupplier8DSequence");
        return runSequence8D.first().isPresent()
                ? runSequence8D.list().get(0).get(SupplierIssueProcessStatuses.SEQUENCE_NUMBER)
                        .toString()
                : "0";
    }

    /**
     * Total row count for 8D
     * 
     * @return
     */
    public String getAllSupplier8DStatusesRowCount() {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getAllSupplier8DStatusesRowCount");
        CqnSelect selectAllEightD = Select.from(SupplierIssueProcessStatuses_.class);
        Result runAllEightD = db.run(selectAllEightD);
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "getAllSupplier8DStatusesRowCount");
        return runAllEightD.first().isPresent() ? String.valueOf(runAllEightD.list().size()) : "0";
    }

    /**
     * Get the quality stream status
     * @param is8dRelevant
     * 
     * @param currentBOStatus
     * @return
     */
    public Result getQualityStreamStatus(int qnSequenceNumber, boolean is8dRelevant) {
        LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getQualityStreamStatus");
        int maxQnSequenceNumber = qnSequenceNumber==getQualityStreamRowCount()&&is8dRelevant ? qnSequenceNumber-1 : qnSequenceNumber;
        logger.info("getQualityStreamStatus maxQnSequenceNumber :: {} ",maxQnSequenceNumber);
        CqnSelect select = Select.from(QualityStreamStatuses_.class)
                .columns(QualityStreamStatuses.STREAM_STATUS_CODE)
                .where(qlty -> qlty.sequenceNumber().eq(maxQnSequenceNumber));
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "getQualityStreamStatus");
       return db.run(select);
         
    }

    /**
     * Get the quality stream status
     * for both quality notification AND Supplier 8D
     * (Only for closed status)
     * @param qnSupplierIssueProcessIdMap
     * @return
     */
	public Result getQualityStreamStatusQNEightD(Map<String, Object> qnSupplierIssueProcessIdMap) {
		LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getQualityStreamStatusQNEightD");
		String currentBOStatusQN = qnSupplierIssueProcessIdMap.get(Constants.QUALITYNOTIFICATION_CODE).toString();
		String currentBOStatus8D = qnSupplierIssueProcessIdMap.get(Constants.SUPPLIER_EIGHTD_CODE).toString();
		int qnSequenceNumber = getQualitySequenceNumber(currentBOStatusQN);
		logger.info("Quality Stream Sequence Number for QN Status is generated");

		int supplierIssueSequenceNumber = getSupplier8DSequenceNumber(currentBOStatus8D);
		logger.info("Quality Stream Sequence Number for S8D Status is created");
		return findStreamStatusByMaximumSequence(qnSequenceNumber, supplierIssueSequenceNumber);

	}
    
	/**
	 * Get the quality stream status for both quality notification OR Supplier 8D
	 * (Except for closed status)
	 * 
	 * @param qnSupplierIssueProcessIdMap
	 * @return
	 */
	public Result getQualityStreamStatusQNOrEightD(Map<String, Object> qnSupplierIssueProcessIdMap) {
		LoggerHelper.logMethodEntry(logger, STREAM_DAO, "getQualityStreamStatusQNOrEightD");
		String currentBOStatusQN = qnSupplierIssueProcessIdMap.get(Constants.QUALITYNOTIFICATION_CODE).toString();
        logger.info("currentBOStatusQN ::: {} ",currentBOStatusQN);
		String currentBOStatus8D = qnSupplierIssueProcessIdMap.get(Constants.SUPPLIER_EIGHTD_CODE).toString();
        logger.info("currentBOStatus8D ::: {} ",currentBOStatus8D);
		int qnSequenceNumber = getQualitySequenceNumber(currentBOStatusQN);
		logger.info("Quality Stream Sequence Number for QN Status is generated");
        boolean is8DRelevant = null!=(Boolean) qnSupplierIssueProcessIdMap.get("isRelevant")
                              && (Boolean) qnSupplierIssueProcessIdMap.get("isRelevant");
        logger.info("[StreamDao] is8DRelevant ::: {} ",is8DRelevant);
		if(qnSequenceNumber>=1 && StringUtils.isNotBlank(currentBOStatus8D)) {
			int supplierIssueSequenceNumber = getSupplier8DSequenceNumber(currentBOStatus8D);
			logger.info("Quality Stream Sequence Number for S8D Status is generated");
			qnSequenceNumber = qnSequenceNumber==getQualityStreamRowCount() ? qnSequenceNumber-1 : qnSequenceNumber;
			return findStreamStatusByMaximumSequence(qnSequenceNumber, supplierIssueSequenceNumber);
		}else {
			return getQualityStreamStatus(qnSequenceNumber,is8DRelevant);
		}

	}
    
	/**
	 * Find maximum sequence number and fetch corresponding quality stream status
	 * @param qnSequenceNumber
	 * @param supplierIssueSequenceNumber
	 * @return
	 */
	public Result findStreamStatusByMaximumSequence(int qnSequenceNumber, int supplierIssueSequenceNumber) {
		int maxStreamSequenceNumber = findMaximumSequenceNumber(qnSequenceNumber,supplierIssueSequenceNumber);
		logger.info("Maximum of QN and S8D status sequence number : ", maxStreamSequenceNumber);
		CqnSelect select = Select.from(QualityStreamStatuses_.class).columns(QualityStreamStatuses.STREAM_STATUS_CODE)
				.where(qlty->qlty.sequenceNumber().eq(maxStreamSequenceNumber));
		LoggerHelper.logMethodExit(logger, STREAM_DAO, "getQualityStreamStatusQNOrEightD");
		return db.run(select);
	}
	
	/**
	 * Get the sequence number for the Supplier 8D status
	 * @param qnSequenceNumber
	 * @param supplierIssueSequenceNumber
	 * @return
	 */
	public int findMaximumSequenceNumber(int qnSequenceNumber, int supplierIssueSequenceNumber) {
		int maxStreamSequenceNumber = 0;
		int qualityStreamRowCount = getQualityStreamRowCount();
		logger.info("Quality Stream status Row Count is received");
		if(qualityStreamRowCount==qnSequenceNumber) {
			if(supplierIssueSequenceNumber>0) {
				maxStreamSequenceNumber = supplierIssueSequenceNumber<(qualityStreamRowCount-1) ? supplierIssueSequenceNumber : qnSequenceNumber;
			}			
		}else {
			maxStreamSequenceNumber = Math.max(qnSequenceNumber, supplierIssueSequenceNumber);
		}
		return maxStreamSequenceNumber;
	}
    
	/**
	 * Get the sequence number for the Supplier 8D status
	 * @param currentBOStatus8D
	 * @return
	 */
	public int getSupplier8DSequenceNumber(String currentBOStatus8D) {
		Result supplierIssueSequenceResult = db
				.run(Select.from(QualityStreamStatuses_.class).columns(QualityStreamStatuses.SEQUENCE_NUMBER)
						.where(qlty -> qlty.supplierIssueProcessStatus_code().eq(currentBOStatus8D)));
		return supplierIssueSequenceResult.first().isPresent()
				? Integer.parseInt(supplierIssueSequenceResult.list().get(0).get(QualityStreamStatuses.SEQUENCE_NUMBER).toString())
				: 0;
	}
    
	/**
	 * Get the sequence number for the QN status
	 * @param currentBOStatusQN
	 * @return
	 */
	public int getQualitySequenceNumber(String currentBOStatusQN) {
		Result qnSequenceResult = db
				.run(Select.from(QualityStreamStatuses_.class).columns(QualityStreamStatuses.SEQUENCE_NUMBER)
						.where(qlty -> qlty.qualityNotificationStatus_code().eq(currentBOStatusQN)));
		return qnSequenceResult.first().isPresent()
				? Integer.parseInt(qnSequenceResult.list().get(0).get(QualityStreamStatuses.SEQUENCE_NUMBER).toString())
				: 0;
	}
	
	/**
	 * Get the total row count of Quality Streams
	 * @return
	 */
	public int getQualityStreamRowCount() {
		Result qnSequenceResult = db
				.run(Select.from(QualityStreamStatuses_.class));
		return qnSequenceResult.first().isPresent()
				? qnSequenceResult.list().size()
				: 0;
	}

    /**
     * Get the stream level status of claim based on its current BO level status
     * 
     * @param currentBOStatus
     * @return
     */
    public Result findLogisticsStreamStatus(String currentBOStatus) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "findLogisticsStreamStatus");
        CqnSelect select = Select.from(LogisticsStreamStatuses_.class)
                .columns(LogisticsStreamStatuses.STREAM_STATUS_CODE)
                .where(log -> log.returnPurchaseOrderStatus_code().eq(currentBOStatus));
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "findLogisticsStreamStatus");
       return db.run(select);
        
    }

    /**
     * Update stream with the latest stream status code
     * 
     * @param streamId
     * @param streams
     */
    public void updateStreams(String streamId, Streams streams) {
    	LoggerHelper.logMethodEntry(logger, STREAM_DAO, "updateStreams");
        logger.info("Updating stream status!! ");
        CqnUpdate update = Update.entity(Streams_.class).data(streams)
                .where(s -> s.ID().eq(streamId));
        db.run(update);
        LoggerHelper.logMethodExit(logger, STREAM_DAO, "updateStreams");
    }
    /**
     * Create Streams
     * 
     * @param stream
     * @return Streams
     */
    public Result createStream(Streams stream) {
    	return db.run(Insert.into(Streams_.class).entry(stream));
    }



    /**
     * Get Streams types
     * 
     * @return List<StreamTypes>
     */
    public Result getStreamType() {
        return db.run(Select.from(StreamTypes_.class));

    }


    /**
     * Get BusinessObject types
     * 
     * @return List<BusinessObjectTypes>
     */
    public Result getBusinessObjectTypes() {
        return db.run(Select.from(BusinessObjectTypes_.class));
    }



    /**
     * Insert Business Objects
     * 
     * @param bo
     * @return BusinessObjects
     */
    public BusinessObjects insertBusinessObjects(BusinessObjects bo) {
        return db.run(Insert.into(BusinessObjects_.class).entry(bo)).single(BusinessObjects.class);
    }



    /**
     * Check if BusinessObject has already performed with the respective actions ( hence the status
     * has been updated)
     * 
     * @param complaint
     * @param boType
     * @param boStatus
     * @return Result
     */
    public Result getBusinessObjectsWithStatus(String complaint, String boType, String boStatus) {
        return db.run(Select.from(BusinessObjectMappings_.class).where(d -> d
                .get(BusinessObjectMappings.COMPLAINT_ID).eq(complaint)
                .and(d.get(BusinessObjectMappings.BUSINESS_OBJECT_TYPE).eq(boType))
                .and(d.get(BusinessObjectMappings.BUSINESS_OBJECT_STATUS_CODE).eq(boStatus))));

    }

    /**
     * get BusinessObject based on ID
     * 
     * @param businessObjectID
     * @return Result
     */
    public Result getBusinessObject(String businessObjectID) {
        return db.run(Select.from(BusinessObjects_.class)
                .where(d -> d.get(BusinessObjects.ID).eq(businessObjectID)));
        

    }

    /**
     * get Stream based on ID
     * 
     * @param id
     * @return Result
     */

    public Result getStream(String id) {
        return db.run(Select.from(Streams_.class)
                .columns(Streams_::parentID_ID,
                        d -> d.businessObjects().expand(BusinessObjects_::businessObjectID_ID,
                        		BusinessObjects_::businessObjectType_code),
                        Streams_::streamType_code,Streams_::status_code,Streams_::isRelevant)
                .where(d -> d.get(Streams.ID).eq(id)));
    }
    
	/**
	 * Check the status for all stream
	 * 
	 * @param complaintId
	 */
	public Result checkAllStreamStatus(String complaintId) {
		return db.run(
				Select.from(Streams_.class).where(s -> s.parentID_ID().eq(complaintId).and(s.isRelevant().eq(true))));

	}
    /**
     * Get the sequence number for streams based on complaint type and stream type 
     * @param streamTypeCode
     * @param complaintType
     * @return
     */
	public Result getSequenceBasedOnComplaintStreamType(String streamTypeCode, String complaintType) {
		 return db.run(Select.from(ComplaintTypeStreamTypeMappings_.class)
	                .where(comp->comp.streamType_code().eq(streamTypeCode).and(comp.complaintType_code().eq(complaintType))));
		
	}

}
