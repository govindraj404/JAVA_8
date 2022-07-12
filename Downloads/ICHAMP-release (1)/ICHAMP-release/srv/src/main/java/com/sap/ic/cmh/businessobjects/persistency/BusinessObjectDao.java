package com.sap.ic.cmh.businessobjects.persistency;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes;
import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes_;
import cds.gen.com.sap.ic.cmh.commonclaimstatusmapping.CommonClaimStatusMappings;
import cds.gen.com.sap.ic.cmh.commonclaimstatusmapping.CommonClaimStatusMappings_;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings;
import cds.gen.com.sap.ic.cmh.qualitynotificationstatusmapping.QualityNotificationStatusMappings_;
import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings;
import cds.gen.com.sap.ic.cmh.returnpurchaseorderstatusmapping.ReturnPurchaseOrderStatusMappings_;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatusmapping.SupplierIssueProcessStatusMappings_;
import cds.gen.complaintservice.BusinessObjectStatuses;
import cds.gen.complaintservice.BusinessObjectStatuses_;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.BusinessObjects_;
import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.complaintservice.CommonBusinessObjects_;
import cds.gen.qualitynotificationservice.BusinessObjectRelations;
import cds.gen.qualitynotificationservice.BusinessObjectRelations_;

@Repository
public class BusinessObjectDao {

	@Autowired
	PersistenceService db;
	
	public static final Logger logger = LoggerHelper.getLogger(BusinessObjectDao.class);

	/**
	 * Link BO id for a particular Complaint and BO type
	 * 
	 * @param businessObjects
	 * @param complaintId
	 * @param boType
	 */
	public void updateBusinessObject(BusinessObjects businessObjects, String complaintId, String boType) {
		CqnUpdate update = Update.entity(BusinessObjects_.class).data(businessObjects)
				.where(b -> b.complaint().eq(complaintId).and(b.businessObjectType_code().eq(boType)));
		db.run(update);
	}

	/**
	 * For status from backend system, find corresponding claim status to be
	 * displayed in cockpit
	 * 
	 * @param backendStatusCode
	 * @return
	 */
	public Result findClaimStatusMappingbyCode(String backendStatusCode) {
		CqnSelect select = Select.from(CommonClaimStatusMappings_.class).columns(CommonClaimStatusMappings.STATUS_CODE)
				.where(csm -> csm.code().eq(backendStatusCode)).orderBy(b -> b.value().asc());
		return db.run(select);
	}

	/**
	 * For status from backend system, find corresponding QN status to be displayed
	 * in cockpit
	 * 
	 * @param backendStatusCode
	 * @return
	 */
	public Result findQNStatusMappingbyCode(String backendStatusCode) {
		CqnSelect select = Select.from(QualityNotificationStatusMappings_.class)
				.columns(QualityNotificationStatusMappings.STATUS_CODE).where(qnm -> qnm.code().eq(backendStatusCode));
		return db.run(select);	
	}

	/**
	 * For status from backend system, find corresponding EightD status to be
	 * displayed in cockpit
	 * 
	 * @param backendStatusCode
	 * @return
	 */
	public Result findEightDStatusMappingbyCode(String backendStatusCode,String fieldName) {
		CqnSelect select = Select.from(SupplierIssueProcessStatusMappings_.class)
				.columns(SupplierIssueProcessStatusMappings.STATUS_CODE).where(sdm -> sdm.code().eq(backendStatusCode)
						.and(sdm.fieldName().eq(fieldName)));
		return db.run(select);
	}

	/**
	 * For status from backend system, find corresponding RPO status to be displayed
	 * in cockpit
	 * 
	 * @param backendStatusCode
	 * @return
	 */
	public Result findReturnPOStatusMappingbyCode(String backendStatusCode) {
		CqnSelect select = Select.from(ReturnPurchaseOrderStatusMappings_.class)
				.columns(ReturnPurchaseOrderStatusMappings.STATUS_CODE).where(rpm -> rpm.code().eq(backendStatusCode));
		return db.run(select);
	}

	/**
	 * Insert BusinessObectStatus into draft entity with the current BO status
	 * 
	 * @param businessObjectStatus
	 */
	public void insertBusinessObjectStatuses(BusinessObjectStatuses businessObjectStatus) {
		CqnInsert insert = Insert.into(BusinessObjectStatuses_.class).entry(businessObjectStatus);
		db.run(insert);
	}
	
	/**
	 * Insert BusinessObectStatus into active entity with the current BO status
	 * 
	 * @param businessObjectStatus
	 */
	public void insertBusinessObjectStatusesActive(cds.gen.managecomplaintservice.BusinessObjectStatuses businessObjectStatus) {
		CqnInsert insert = Insert.into(cds.gen.managecomplaintservice.BusinessObjectStatuses_.class).entry(businessObjectStatus);
		db.run(insert);
	}

	public Result getBusinessObjectsBasedOnBusinessObjectId(String businessObjectId) {
		CqnSelect select = Select.from(BusinessObjects_.class)
				.columns(BusinessObjects.STREAM_ID, BusinessObjects.COMPLAINT)
				.where(b -> b.businessObjectID_ID().eq(businessObjectId));
		return db.run(select);
	}

	/**
	 * Get the current/latest status of a BO based on its ID Sorting by modifiedAt,
	 * since there might be multiple statuses for a BO
	 * 
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getCurrentBOStatus(String businessObjectIDId) {
		CqnSelect select = Select.from(BusinessObjectStatuses_.class)
				.columns(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE)
				.where(b -> b.parent().eq(businessObjectIDId)).orderBy(b -> b.businessObjectStatus().sequenceNumber().desc());
		return db.run(select);

	}
	
	/**
	 * Get the current/latest status of a BO based on its ID Sorting by modifiedAt,
	 * since there might be multiple statuses for a BO
	 * 
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getCurrentActiveBOStatus(String businessObjectIDId) {
		CqnSelect select = Select.from(cds.gen.managecomplaintservice.BusinessObjectStatuses_.class)
				.columns(cds.gen.managecomplaintservice.BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE)
				.where(b -> b.parent().eq(businessObjectIDId)).orderBy(b -> b.businessObjectStatus().sequenceNumber().desc());
		return db.run(select);

	}
	

	/**
	 * Get the current/latest status of a BO based on its ID and type Sorting by
	 * modifiedAt, since there might be multiple statuses for a BO
	 * 
	 * @param boType2
	 * 
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getBusinessObjectStatusBasedOnType(String complaintId, String boType) {
		Result run = getBusinessObjectIdBasedOnTypeAndComplaint(complaintId, boType);
		Object object = run.list().get(0).get(BusinessObjects.BUSINESS_OBJECT_ID_ID);
		String businessObjectId = run.first().isPresent() && null != object ? object.toString() : "";
		if(StringUtils.isNotBlank(businessObjectId)) {
			Result currentBOStatusSupplier8DResult = getCurrentBOStatusSupplier8D(businessObjectId);
			return null!=currentBOStatusSupplier8DResult && currentBOStatusSupplier8DResult.first().isPresent() 
					? currentBOStatusSupplier8DResult : getCurrentActiveBOStatusSupplier8D(businessObjectId);
		}else { 
			return null;
		}
	}

    /**
	 * Get BusinessObectId based on boType and complaint id
	 * @param complaintId
	 * @param boType
	 * @return
	 */
	public Result getBusinessObjectIdBasedOnTypeAndComplaint(String complaintId, String boType){
		CqnSelect select = Select.from(BusinessObjects_.class).columns(BusinessObjects.BUSINESS_OBJECT_ID_ID)
				.where(b -> b.businessObjectType_code().eq(boType).and(b.complaint().eq(complaintId)));
		return db.run(select);
	}
	
	/**
	 * Get the current/latest status of a BO based on its ID and type Sorting by
	 * modifiedAt, since there might be multiple statuses for a BO
	 * 
	 * @param boType2
	 * 
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getActiveBusinessObjectStatusBasedOnType(String complaintId, String boType) {
		CqnSelect select = Select.from(BusinessObjects_.class).columns(BusinessObjects.BUSINESS_OBJECT_ID_ID)
				.where(b -> b.businessObjectType_code().eq(boType).and(b.complaint().eq(complaintId)));
		Result run = db.run(select);
		Object object = run.list().get(0).get(BusinessObjects.BUSINESS_OBJECT_ID_ID);
		String businessObjectId = run.first().isPresent() && null != object ? object.toString() : "";
		return StringUtils.isNotBlank(businessObjectId) ? getCurrentActiveBOStatus(businessObjectId) : null;

	}

	/**
	 * Fetch the respective stream type of a BO type
	 * 
	 * @param boType
	 * @return
	 */
	public Result fetchStreamTypeByBOType(String boType) {
		CqnSelect select = Select.from(BusinessObjectTypes_.class).columns(BusinessObjectTypes.STREAM_TYPE_CODE)
				.where(b -> b.code().eq(boType));
		return db.run(select);	
	}

	/**
	 * Insert Business Object Relations to create Document flow in UI
	 * 
	 * @param businessObjectRelations
	 */
	public void insertBusinessObjectRelations(BusinessObjectRelations businessObjectRelations) {
		CqnInsert insert = Insert.into(BusinessObjectRelations_.class).entry(businessObjectRelations);
		db.run(insert);
	}
    
	/**
	 * Check if Business Object is relevant
	 * @param complaintId
	 * @param boType
	 * @return
	 */
	public Result checkIfBOIsRelevant(String complaintId, String boType) {
		CqnSelect select = Select.from(BusinessObjects_.class).columns(BusinessObjects.IS_RELEVANT)
				.where(b -> b.businessObjectType_code().eq(boType).and(b.complaint().eq(complaintId)));
		return db.run(select);	
	}
    
	/**
	 * Check if business object has status "created" in BusinessObjectStatuses (Active entity)
	 * @param createdStatusCode
	 * @param businessObjectId
	 * @return
	 */
	public Result checkIfCreatedStatusExistsActive(String createdStatusCode, String businessObjectId) {
		return db.run(Select.from(cds.gen.managecomplaintservice.BusinessObjectStatuses_.class)
				.where(b -> b.parent().eq(businessObjectId).and(b.businessObjectStatus_code().eq(createdStatusCode))));
		
	}
	
	/**
	 * Check if business object has status "created" in BusinessObjectStatuses (Draft entity)
	 * @param createdStatusCode
	 * @param businessObjectId
	 * @return
	 */
	public Result checkIfCreatedStatusExists(String createdStatusCode, String businessObjectId) {
		return db.run(Select.from(BusinessObjectStatuses_.class)
				.where(b -> b.parent().eq(businessObjectId).and(b.businessObjectStatus_code().eq(createdStatusCode))));
		
    }
    
    /**
	 * Get all the Return Purchase Order Status Mappings
	 * in cockpit
	 * @return
	*/
	public List<ReturnPurchaseOrderStatusMappings> getAllReturnPOStatusMappings() {
		CqnSelect select = Select.from(ReturnPurchaseOrderStatusMappings_.class);
		return db.run(select).listOf(ReturnPurchaseOrderStatusMappings.class);
    }
    
    /**
	 * Find the BO Status based on the Business Object ID and Status Code
	 * @param businessObjectIDId
     * @param statusCode
	 * @return
	 */
	public Result findBOStatusCode(String businessObjectIDId, String statusCode) {
		CqnSelect select = Select.from(BusinessObjectStatuses_.class)
				.columns(BusinessObjectStatuses.ID, BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE, BusinessObjectStatuses.PARENT)
				.where(b -> b.parent().eq(businessObjectIDId).and(b.businessObjectStatus_code().eq(statusCode)));
		return db.run(select);
	}
	
	public List<SupplierIssueProcessStatusMappings> getAllSupplierIssueProcessMappings() {
		CqnSelect select = Select.from(SupplierIssueProcessStatusMappings_.class);
		return db.run(select).listOf(SupplierIssueProcessStatusMappings.class);
    }
	
	/**
	 * Get the current/latest status for supplier 8D based on its ID Sorting by sequence number
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getCurrentActiveBOStatusSupplier8D(String businessObjectIDId) {
		CqnSelect select = Select.from(cds.gen.managecomplaintservice.BusinessObjectStatuses_.class)
				.columns(cds.gen.managecomplaintservice.BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE)
				.where(b -> b.parent().eq(businessObjectIDId)).orderBy(b -> b.businessObjectStatus().sequenceNumber().desc());
		return db.run(select);

	}
	
	/**
	 * Get the current/latest status for supplier 8D based on its ID Sorting by sequence number
	 * @param businessObjectIDId
	 * @return
	 */
	public Result getCurrentBOStatusSupplier8D(String businessObjectIDId) {
		CqnSelect select = Select.from(BusinessObjectStatuses_.class)
				.columns(BusinessObjectStatuses.BUSINESS_OBJECT_STATUS_CODE)
				.where(b -> b.parent().eq(businessObjectIDId)).orderBy(b -> b.businessObjectStatus().sequenceNumber().desc());
		return db.run(select);

	}

	public List<CommonBusinessObjects> getCommonBusinessObjectsBasedOnComplaint(String complaintID){
		CqnSelect select = Select.from(CommonBusinessObjects_.class).where(b->b.complaint_ID().eq(complaintID));
		return db.run(select).listOf(CommonBusinessObjects.class);
	}

	public List<CommonBusinessObjects> getCommonBusinessObjectsBasedOnBusinessPartner(String businessPartnerID){
		CqnSelect select = Select.from(CommonBusinessObjects_.class).where(b->b.supplier_ID().eq(businessPartnerID).or(b.personResponsible_ID().eq(businessPartnerID)).or(b.contactPerson_ID().eq(businessPartnerID)));
		return db.run(select).listOf(CommonBusinessObjects.class);
	}
	
	/**
	 * Get all quality notification status mappings
	 */
	public List<QualityNotificationStatusMappings> getAllQualityNotificationStatusMappings() {
		CqnSelect select = Select.from(QualityNotificationStatusMappings_.class);
		return db.run(select).listOf(QualityNotificationStatusMappings.class);
    }

    /**
	 * Find the BO Status based on the Business Object ID and Status Code
	 * @param businessObjectIDId
     * @param statusCode
	 * @return
	 */
	public Result findBOStatusBasedOnBOId(String businessObjectIDId) {
		CqnSelect select = Select.from(BusinessObjectStatuses_.class)
				.where(b -> b.parent().eq(businessObjectIDId));
		return db.run(select);
	}
}