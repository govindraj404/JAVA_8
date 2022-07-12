package com.sap.ic.cmh.qualitynotification.persistency;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.qualitynotificationservice.Defects_;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.com.sap.ic.cmh.defectcode.DefectCodes_;
import cds.gen.com.sap.ic.cmh.defectgroup.DefectGroups_;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.qualitynotificationservice.QualityNotifications_;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;

@Repository
public class QualityNotificationDao {

	private DraftService draftService;

	QualityNotificationDao(@Qualifier("QualityNotificationService") DraftService draftService, PersistenceService db) {
		this.draftService = draftService;
		this.db = db;
	}

	@Autowired
	PersistenceService db;

	public static final Logger logger = LoggerHelper.getLogger(QualityNotificationDao.class);

	/**
	 * Update Quality notification
	 * 
	 * @param qn
	 */
	public void updateQualityNotification(QualityNotifications qn) {
		db.run(Update.entity(QualityNotifications_.class).data(qn)).single(QualityNotifications.class);
	}

	/**
	 * Get the Quality notification details based on ID
	 * 
	 * @param qnId
	 * @return
	 */
	public Result getQualityNotificationDetails(String qnId) {
		CqnSelect select = Select.from(QualityNotifications_.class).where(qn -> qn.ID().eq(qnId));
		return db.run(select);
	}

	/**
	 * Get the Quality notification details based on complaint ID
	 * 
	 * @param complaintId
	 * @return
	 */
	public Result getQualityNotificationDetailsByComplaintId(String complaintId) {
		CqnSelect select = Select.from(QualityNotifications_.class)
				.columns(QualityNotifications.ID, QualityNotifications.IDENTIFIER)
				.where(qn -> qn.complaint_ID().eq(complaintId));
		return db.run(select);
	}

	/**
	 * Get the Quality notification details based on QN Number
	 * 
	 * @param qnNumber
	 * @return
	 */
	public Result checkIfQNExistsBasedOnNumber(String qnNumber) {
		CqnSelect select = Select.from(QualityNotifications_.class)
				.columns(QualityNotifications.ID, QualityNotifications.IDENTIFIER, QualityNotifications.COMPLAINT_ID)
				.where(qn -> qn.identifier().eq(qnNumber));
		return db.run(select);
	}

	/**
	 * Get the QN number,PO and defect code details for supplier 8D creation
	 * 
	 * @param complaintId
	 * @return
	 */
	public QualityNotifications fetchQNForSupplier8d(String complaintId) {
		CqnSelect select = Select.from(QualityNotifications_.class)
				.columns(a -> a.identifier(), b -> b.purchaseOrderNumber(), c -> c.purchaseOrderItem(),
						d -> d.defect().expand(c -> c.ID(), c -> c.identifier(), c -> c.defectGroup_code(),
								e -> e.defectCode_code()))
				.where(qn -> qn.complaint_ID().eq(complaintId));
		return db.run(select).single(QualityNotifications.class);
	}

	/**
	 * Get the defect code based on code
	 * 
	 * @param defectCode
	 * @return
	 */
	public Result getDefectCode(String defectCode) {
		CqnSelect select = Select.from(DefectCodes_.class).where(dc -> dc.code().eq(defectCode));
		return db.run(select);
	}

	/**
	 * Get the defect group based on code
	 * 
	 * @param defectGroup
	 * @return
	 */
	public Result getDefectGroup(String defectGroup) {
		CqnSelect select = Select.from(DefectGroups_.class).where(dc -> dc.code().eq(defectGroup));
		return db.run(select);
	}

	/**
	 * Get the defect details based on QN ID
	 * 
	 * @param qnId
	 * @return
	 */
	public Result getDefectBasedOnQN(String qnId) {
		CqnSelect select = Select.from(Defects_.class).where(dc -> dc.parent_ID().eq(qnId));
		return db.run(select);
	}

	/**
	 * Delete duplicate quality notification
	 * 
	 * @param complaintId
	 */
	public void deleteAutomaticQN(String complaintId) {
		CqnDelete delete = Delete.from(QualityNotifications_.class).where(qn -> qn.complaint_ID().eq(complaintId));
		long rowCount = db.run(delete).rowCount();
		logger.info("Quality Notification deleted count: {} ", rowCount);

	}

	/**
	 * Get Status code and company code based on QN ID
	 */
	public Result getStatusAndCompanyCode(String qualityNotficationId) {
		CqnSelect select = Select.from(QualityNotifications_.class)
				.columns(QualityNotifications.STATUS_CODE, QualityNotifications.COMPANY_ID)
				.where(qn -> qn.ID().eq(qualityNotficationId));
		return db.run(select);
	}

	/**
	 * Read draft Quality Notification based on Complaint ID
	 * 
	 * @param complaintId
	 * @return Quality Notifications
	 */
	public Result getDraftQNByComplaintID(String complaintId) {
		return draftService.run(Select.from(QualityNotifications_.class)
				.where(b -> b.get(QualityNotifications.COMPLAINT_ID).eq(complaintId)
						.and(b.IsActiveEntity().eq(false))));
	}

	/**
	 * Delete draft QN based on QN ID
	 * 
	 * @param qualityNotificationId
	 */

	public void deleteDraftQNByID(String qualityNotificationId) {
		draftService.cancelDraft(
				Delete.from(QualityNotifications_.class).where(b -> b.ID().eq(qualityNotificationId)));
	}

	/**
	 * Read draft Defect based on QualityNotification ID
	 * 
	 * @param qualityNotificationId
	 * @return Defects
	 */
	public Result getDraftDefectByQualityNotificationID(String qualityNotificationId) {
		return draftService.run(Select.from(Defects_.class)
				.where(b -> b.get(Defects.PARENT_ID).eq(qualityNotificationId)
						.and(b.IsActiveEntity().eq(false))));
	}

    /**
	 * Get the Quality notification details based on ID
	 * 
	 * @param qnId
	 * @return
	 */
	public Result getQualityNotificationDetailsFromActive(String qnId) {
		CqnSelect select = Select.from(cds.gen.managequalitynotificationservice.QualityNotifications_.class)
				.where(qn -> qn.ID().eq(qnId));
		return db.run(select);
	}

    /**
	 * Get active defect details based on QN ID
	 * 
	 * @param qnId
	 * @return
	 */
	public Result getDefectBasedOnQNFromActive(String qnId) {
		CqnSelect select = Select.from(cds.gen.managequalitynotificationservice.Defects_.class).where(dc -> dc.parent_ID().eq(qnId));
		return db.run(select);
	}

	/**
	 * Get the Quality notification details based on QN Number
	 * 
	 * @param qnNumber
	 * @return
	 */
	public Result checkIfActiveQNExistsBasedOnNumber(String qnNumber) {
		CqnSelect select = Select.from(cds.gen.managequalitynotificationservice.QualityNotifications_.class)
				.columns(QualityNotifications.ID)
				.where(qn -> qn.identifier().eq(qnNumber));
		return db.run(select);
	}

}
