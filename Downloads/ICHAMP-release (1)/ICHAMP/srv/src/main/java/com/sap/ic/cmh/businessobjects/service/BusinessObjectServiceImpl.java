package com.sap.ic.cmh.businessobjects.service;

import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes;
import cds.gen.com.sap.ic.cmh.commonclaimstatusmapping.CommonClaimStatusMappings;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings;
import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.complaintservice.BusinessObjectStatuses;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.qualitynotificationservice.BusinessObjectRelations;

@Service
public class BusinessObjectServiceImpl implements BusinessObjectService {

	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	BusinessObjectDao businessObjectDao;

	private static final String BUSINESS_OBJECT_SERVICE_IMPL = "BusinessObjectServiceImpl";
	private static final Logger logger = LoggerFactory.getLogger(BusinessObjectServiceImpl.class);
	private static final String SET_BUSINESS_OBJECT_STATUS = "setBusinessObjectStatus";

	@Override
	/**
	 * Set Business Object status after creating a Bo Invoke from respective
	 * handlers
	 */
	public void updateBusinessObjects(Object businessObject, String boType) {
		// convert object to a map
		Map<String, Object> genericBusinessObjectMap = commonFunctions.convertObjectToMap(businessObject);
		// for a particular boType and complaint id, update the BO id in BusinessObject
		// entity
		updateBusinessObject(boType, genericBusinessObjectMap);

	}
	/**
	 * Update Business object for a Complaint and BO type
	 * 
	 * @param boType
	 * @param genericBusinessObjectMap
	 */
	public String updateBusinessObject(String boType, Map<String, Object> genericBusinessObjectMap) {
		LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_SERVICE_IMPL, "updateBusinessObject");
		BusinessObjects businessObjects = Struct.create(BusinessObjects.class);
		String businessObjectId = genericBusinessObjectMap.get(Constants.ID).toString();
		businessObjects.setBusinessObjectIDId(businessObjectId);
		String complaintId = genericBusinessObjectMap.get(Constants.COMPLAINT_ID).toString();
		businessObjectDao.updateBusinessObject(businessObjects, complaintId, boType);
		logger.info("Business Object is updated successfully for the BOtype and complaint");
		LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_SERVICE_IMPL, "updateBusinessObject");
		return businessObjectId;

	}

	/**
	 * Get the corresponding CMH cockpit BO status for the backend status and update
	 * in BusinessObjectStatus entity
	 * 
	 * @param boType
	 * @param backendStatusCode
	 */
	@Override
	public void setBusinessObjectStatus(String boType, String backendStatusCode, String businessObjectId,boolean isActive) {
		LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_SERVICE_IMPL, SET_BUSINESS_OBJECT_STATUS);
		logger.info("Business Object type for which BOStatus is inserted{} ", boType);
		String statusCode = "";
		Result statusResult;
		switch (boType) {

		case Constants.CLAIM_CODE:
			statusResult = businessObjectDao.findClaimStatusMappingbyCode(backendStatusCode);
			statusCode = statusResult.first().isPresent()
					? statusResult.list().get(0).get(CommonClaimStatusMappings.STATUS_CODE).toString()
					: "";
			validateIfStatusIsCreated(boType, businessObjectId, isActive, statusCode,Constants.CLAIM_CREATED);
			logger.info("Claim's corresponding cockpit status  {}", statusCode);
			break;
		case Constants.QUALITYNOTIFICATION_CODE:
			statusCode = determineQualityNotificationStatus(backendStatusCode);
			validateIfStatusIsCreated(boType, businessObjectId, isActive, statusCode,Constants.QN_STATUS_CREATED);
			logger.info("Quality Notification's corresponding cockpit status  {}", statusCode);
			break;
		case Constants.SUPPLIER_EIGHTD_CODE:
			statusResult = businessObjectDao.findEightDStatusMappingbyCode(backendStatusCode,Constants.LIFECYCLE_STATUS_CODE);
			statusCode = statusResult.first().isPresent()
					? statusResult.list().get(0).get(SupplierIssueProcessStatusMappings.STATUS_CODE).toString()
					: "";
			validateIfStatusIsCreated(boType, businessObjectId, isActive, statusCode,Constants.SUPPLIER_ISSUE_PROCESS_STATUS_CREATED);
			logger.info("Supplier 8D's corresponding cockpit status  {}", statusCode);
			break;
		case Constants.RETURNPO_CODE:
			statusCode = determineReturnOrderStatus(backendStatusCode);
			validateIfStatusIsCreated(boType, businessObjectId, isActive, statusCode,Constants.RPO_STATUS_CREATED);
			logger.info("Return PO's corresponding cockpit status  {}", statusCode);
			break;
		default:
			break;
		}
		insertBusinessObjectStatus(boType, businessObjectId, statusCode,isActive);
		LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_SERVICE_IMPL, SET_BUSINESS_OBJECT_STATUS);
    }
	
	@Override
	public String determineReturnOrderStatus(String backendStatusCode) {
		String statusCode;
		Result statusResult;
		statusResult = businessObjectDao.findReturnPOStatusMappingbyCode(backendStatusCode);
		statusCode = statusResult.first().isPresent()
				? statusResult.list().get(0).get(ReturnPurchaseOrderStatusMappings.STATUS_CODE).toString()
				: "";
		return statusCode;
	}
    
    @Override
    public String determineQualityNotificationStatus(String backendStatusCode) {
		String statusCode;
		Result statusResult;
		statusResult = businessObjectDao.findQNStatusMappingbyCode(backendStatusCode);
		statusCode = statusResult.first().isPresent()
				? statusResult.list().get(0).get(QualityNotificationStatusMappings.STATUS_CODE).toString()
				: "";
		return statusCode;
	}
	/**
	 * Check if the derived status is created
	 * If not created, check if the status exists in BusinessObjectStatuses
	 * @param boType
	 * @param businessObjectId
	 * @param isActive
	 * @param statusCode
	 * @param createdBOStatusCode
	 */
	public void validateIfStatusIsCreated(String boType, String businessObjectId, boolean isActive, String statusCode,String createdBOStatusCode) {
		LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_SERVICE_IMPL, SET_BUSINESS_OBJECT_STATUS);
		if (StringUtils.isNotBlank(statusCode) && !statusCode.equalsIgnoreCase(createdBOStatusCode))
			checkIfCreatedStatusExists(createdBOStatusCode, boType, businessObjectId, isActive);
		LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_SERVICE_IMPL, SET_BUSINESS_OBJECT_STATUS);
	}
    
	/**
	 * Check if Created status of Business object exists in BusinessObjectStatuses
	 * If not, insert the created status information
	 * @param createdStatusCode
	 * @param boType
	 * @param businessObjectId
	 * @param isActive
	 */
	public void checkIfCreatedStatusExists(String createdStatusCode, String boType, String businessObjectId,
			boolean isActive) {
		LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_SERVICE_IMPL, "validateIfStatusIsCreated");
		Result checkIfCreatedStatusExistsResult = isActive
				? businessObjectDao.checkIfCreatedStatusExistsActive(createdStatusCode, businessObjectId)
				: businessObjectDao.checkIfCreatedStatusExists(createdStatusCode, businessObjectId);
		if (!checkIfCreatedStatusExistsResult.first().isPresent()) {
            insertBusinessObjectStatus(boType, businessObjectId, createdStatusCode, isActive);
            logger.info("Business Object status inserted : "+createdStatusCode);
		}
		LoggerHelper.logMethodExit(logger, BUSINESS_OBJECT_SERVICE_IMPL, "validateIfStatusIsCreated");
	}
	
	/**
	 * Insert Business object status
	 * @param boType
	 * @param businessObjectId
	 * @param statusCode
	 * @param isActive
	 */
	@Override
	public void insertBusinessObjectStatus(String boType, String businessObjectId, String statusCode,boolean isActive) {
		LoggerHelper.logMethodEntry(logger, BUSINESS_OBJECT_SERVICE_IMPL, "insertBusinessObjectStatus");
		
		if (StringUtils.isNotBlank(statusCode)) {
			 if(isActive) {
				 cds.gen.managecomplaintservice.BusinessObjectStatuses businessObjectStatus = Struct.create(cds.gen.managecomplaintservice.BusinessObjectStatuses.class);
				    businessObjectStatus.setBusinessObjectType(boType);
					businessObjectStatus.setBusinessObjectStatusCode(statusCode);
					businessObjectStatus.setParent(businessObjectId);
					businessObjectDao.insertBusinessObjectStatusesActive(businessObjectStatus);
			 }else {
				 BusinessObjectStatuses businessObjectStatus = Struct.create(BusinessObjectStatuses.class);
				 businessObjectStatus.setBusinessObjectType(boType);
					businessObjectStatus.setBusinessObjectStatusCode(statusCode);
					businessObjectStatus.setParent(businessObjectId);
					businessObjectDao.insertBusinessObjectStatuses(businessObjectStatus);
			 }
			
			
			logger.info("Business Object status is inserted successfully! {} ", statusCode);
		}
	}

	/**
	 * Fetch the BusinessObject details based on BOType and Complaint id
	 */
	@Override
	public BusinessObjects getBusinessObjectsBasedOnBusinessObjectId(String businessObjectId) {
		Result businessObjectResultList = businessObjectDao.getBusinessObjectsBasedOnBusinessObjectId(businessObjectId);
		return businessObjectResultList.first().isPresent()
				? businessObjectResultList.listOf(BusinessObjects.class).get(0)
				: null;

	}

	/**
	 * Fetch current business Object status by BO ID
	 */
	@Override
	public String getCurrentBOStatus(String businessObjectIDId) {
		Result currentBOStatusResult = businessObjectDao.getCurrentBOStatus(businessObjectIDId);
		return currentBOStatusResult.first().isPresent()
				? currentBOStatusResult.list().get(0).get(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE).toString()
				: "";

	}

	/**
	 * Fetch Stream type based on a business object type
	 */
	@Override
	public String fetchStreamTypeByBOType(String boType) {
		Result fetchStreamTypeResult = businessObjectDao.fetchStreamTypeByBOType(boType);
		return fetchStreamTypeResult.first().isPresent()
				? fetchStreamTypeResult.list().get(0).get(BusinessObjectTypes.STREAM_TYPE_CODE).toString()
				: "";
	}

	/**
	 * Fetch business Object status based on its type
	 */
	@Override
	public String getBusinessObjectStatusBasedOnType(String complaintId,String boType) {
		Result businessObjectIdAndStatusResult = businessObjectDao.getBusinessObjectStatusBasedOnType(complaintId,boType);
		return (null!=businessObjectIdAndStatusResult && businessObjectIdAndStatusResult.first().isPresent())
				? businessObjectIdAndStatusResult.list().get(0).get(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE)
						.toString()
				: "";
	}

	@Override
	public void insertBusinessObjectRelations(String sourceBOType, String qualityNotificationId, String targetBOType,
			String targetId) {
		BusinessObjectRelations businessObjectRelations = Struct.create(BusinessObjectRelations.class);
		businessObjectRelations.setSourceBusinessObjectTypeCode(sourceBOType);
		businessObjectRelations.setTargetBusinessObjectTypeCode(targetBOType);
		businessObjectRelations.setSourceBusinessObjectUUID(qualityNotificationId);
		businessObjectRelations.setTargetBusinessObjectUUIDId(targetId);
		businessObjectDao.insertBusinessObjectRelations(businessObjectRelations);
		
		
		
	}

	/**
	 * Fetch current Active business Object status by BO ID
	 */
	@Override
	public String getCurrentActiveBOStatus(String businessObjectIDId) {
		Result currentBOStatusResult = businessObjectDao.getCurrentActiveBOStatus(businessObjectIDId);
		return currentBOStatusResult.first().isPresent()
				? currentBOStatusResult.list().get(0).get(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE).toString()
				: "";
	}

	/**
	 * Check if Business Object is relevant
	 */
	@Override
	public boolean checkIfBOIsRelevant(String complaintId, String boType) {
		Result checkIfBOIsRelevantResult = businessObjectDao.checkIfBOIsRelevant(complaintId,boType);
		if(checkIfBOIsRelevantResult.first().isPresent()) {
			return (boolean) checkIfBOIsRelevantResult.list().get(0).get(BusinessObjects.IS_RELEVANT);
		}else {
			return false;
		}
	
		
    }
    /**
     * Check if BO status for a Business Object exists
     */
    @Override
    public boolean checkIfBOStatusExists(String businessObjectId, String boStatus) {
     Result checkIfBOStatusExistsResult = businessObjectDao.findBOStatusCode(businessObjectId, boStatus);
     return checkIfBOStatusExistsResult.first().isPresent();
    }
	@Override
	public String getCurrentActiveBOStatusSupplier8D(String businessObjectIDId) {
		Result currentBOStatusResult = businessObjectDao.getCurrentActiveBOStatusSupplier8D(businessObjectIDId);
		return currentBOStatusResult.first().isPresent()
				? currentBOStatusResult.list().get(0).get(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE).toString()
				: "";
	}
	@Override
	public String getCurrentBOStatusSupplier8D(String businessObjectId) {
		Result currentBOStatusResult = businessObjectDao.getCurrentBOStatusSupplier8D(businessObjectId);
		return currentBOStatusResult.first().isPresent()
				? currentBOStatusResult.list().get(0).get(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE).toString()
				: "";
	}

    /**
	 * Get BusinessObectId based on boType and complaint id
	 */
	@Override
	public String getBusinessObjectIdBasedOnTypeAndComplaint(String complaintId,
			String boType) {
		Result run = businessObjectDao.getBusinessObjectIdBasedOnTypeAndComplaint( complaintId, boType);
		Object object = run.list().get(0).get(BusinessObjects.BUSINESS_OBJECT_ID_ID);
		return run.first().isPresent() && null != object ? object.toString() : "";
		
	}

}