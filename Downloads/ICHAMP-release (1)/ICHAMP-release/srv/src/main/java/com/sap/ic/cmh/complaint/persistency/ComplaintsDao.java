package com.sap.ic.cmh.complaint.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.complaintservice.ComplaintStatuses;
import cds.gen.complaintservice.ComplaintStatuses_;
import cds.gen.complaintservice.ComplaintCategories;
import cds.gen.complaintservice.ComplaintCategories_;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

@Repository
public class ComplaintsDao {

	@Autowired
	PersistenceService db;

	private static final String COMPLAINT_DAO = "ComplaintsDao";
	private static final Logger logger = LoggerFactory.getLogger(ComplaintsDao.class);

	/**
	 * Get Default Type
	 * 
	 * @return ComplaintCategories
	 */
	public Result getDefaultType() {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getDefaultType");
		CqnSelect select = Select.from(ComplaintCategories_.class)
				.where(b -> b.get(ComplaintCategories.CODE).eq(Constants.COMPLAINT_TYPE));
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getDefaultType");
		return db.run(select);
	}

	/**
	 * Get Default Status
	 * 
	 * @return ComplaintStatuses
	 */
	public Result getDefaultStatus() {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getDefaultStatus");
		CqnSelect select = Select.from(ComplaintStatuses_.class)
				.where(b -> b.get(ComplaintStatuses.CODE).eq(Constants.COMPLAINT_CREATED_STATUS));
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getDefaultStatus");
		return db.run(select);
	}

	/**
	 * Update Complaint
	 * 
	 * @param complaint
	 */
	public void updateComplaint(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "updateComplaint");
		CqnUpdate complaintUpdate = Update.entity(Complaints_.class).data(complaint);
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "updateComplaint");
		db.run(complaintUpdate);

	}

	/**
	 * Get Complaint details based on complaint id
	 * 
	 * @param complaintId
	 * @return
	 */
	public Result getComplaintDetails(String complaintId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getComplaintDetails");
		CqnSelect select = Select.from(Complaints_.class).where(b -> b.get(Complaints.ID).eq(complaintId));
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getComplaintDetails");
		return db.run(select);
	}

	public Result getAllComplaints() {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getAllComplaints");
		CqnSelect select = Select.from(Complaints_.class).columns(Complaints.ID);
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getAllComplaints");
		return db.run(select);
	}

	/**
	 * Get Active Complaints with the Supplier that is marked to be deleted
	 * 
	 * @param supplierId
	 * @return
	 */
	public Result getActiveComplaintsForSupplier(String supplierId) {
		CqnSelect select = Select.from(Complaints_.class).where(b -> b.supplier_ID().eq(supplierId)
				.and(b.complaintStatus_code().ne(Constants.COMPLAINT_CLOSED)));
		return db.run(select);
	}

	/**
	 * Get Active Complaints with the Person Responsible that is marked to be
	 * deleted
	 * 
	 * @param personResponsibleId
	 * @return
	 */
	public Result getActiveComplaintsForPersonResponsible(String personResponsibleId) {
		CqnSelect select = Select.from(Complaints_.class).where(b -> b.personResponsible_ID().eq(personResponsibleId)
				.and(b.complaintStatus_code().ne(Constants.COMPLAINT_CLOSED)));
		return db.run(select);
	}

	/**
	 * Get Complaints based on the complaint number
	 * 
	 * @param sequenceNumber
	 * @return
	 */
	public Result getComplaintBasedOnCode(String complaintcode) {
		CqnSelect select = Select.from(Complaints_.class).where(b -> b.identifier().eq(complaintcode));
		return db.run(select);
	}

	public boolean getIsComplaintStatusClosedBasedOnSupplier(String businessPartnerId) {
		CqnSelect select = Select.from(Complaints_.class)
				.where(b -> b.supplier_ID().eq(businessPartnerId).and(b.complaintStatus_code().ne("CLSD")));
		Result result = db.run(select);
		return (!result.first().isPresent());
	}

	public boolean getIsComplaintStatusClosedBasedOnContactPerson(String businessPartnerId) {
		CqnSelect select = Select.from(Complaints_.class)
				.where(b -> b.contactPerson_ID().eq(businessPartnerId).and(b.complaintStatus_code().ne("CLSD")));
		Result result = db.run(select);
		return (!result.first().isPresent());
	}

	public void deleteAutomaticComplaint(String complaintId) {
		CqnDelete delete = Delete.from(Complaints_.class).where(bo -> bo.ID().eq(complaintId));
		long rowCount = db.run(delete).rowCount();
		logger.info("Business Objects deleted count: {} ", rowCount);

	}

	/**
	 * Fetch Master Data Attributes of complaint based on ID
	 */
	public Result getMasterDataFromComplaints(String complaintId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getMasterDataFromComplaints");
		CqnSelect select = Select.from(Complaints_.class).columns(Complaints.MATERIAL_ID, Complaints.PLANT_ID,
				Complaints.SUPPLIER_ID, Complaints.PURCHASING_ORGANIZATION_ID, Complaints.QUANTITY,
				Complaints.UNIT_CODE, Complaints.COMPANY_CODE_ID, Complaints.COMPLAINT_TYPE_CODE,
				Complaints.REFERENCE_NUMBER)
				.where(b -> b.get(Complaints.ID).eq(complaintId));
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getMasterDataFromComplaints");
		return db.run(select);
	}

	/**
	 * Fetch Creation type of complaint based on ID
	 */
	public Result getComplaintCreationTypeAndCompanyCode(String complaintId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_DAO, "getComplaintRefNumberAndCreationType");
		CqnSelect select = Select.from(Complaints_.class)
				.columns(Complaints.CREATION_TYPE, Complaints.COMPANY_CODE_ID, Complaints.PURCHASING_ORGANIZATION_ID)
				.where(b -> b.get(Complaints.ID).eq(complaintId));
		LoggerHelper.logMethodExit(logger, COMPLAINT_DAO, "getComplaintRefNumberAndCreationType");
		return db.run(select);
	}
	
	/**
	 * Get complaint category based on code
	 */
	public Result getComplaintCategoryBasedOnCode(String code) {
		return db.run(Select.from(ComplaintCategories_.class).columns(c->c.code())
				.where(b -> b.code().eq(code)));
	}

}
