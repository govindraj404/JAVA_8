package com.sap.ic.cmh.qualitynotification.handler;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.request.UserInfo;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.qualitynotification.validations.QualityNotificationValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.QnValidation;

import cds.gen.complaintservice.Complaints;
import cds.gen.qualitynotificationservice.BusinessObjectStatuses;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.qualitynotificationservice.QualityNotifications_;
import cds.gen.qualitynotificationservice.Addresses_;
import cds.gen.qualitynotificationservice.BusinessPartners_;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;

@Component
@ServiceName("QualityNotificationService")
public class QualityNotificationHandler implements EventHandler {

	@Autowired
	QualityNotificationService qualityNotificationService;

	@Autowired
	ComplaintService complaintService;
	@Autowired
	AuditLogHelper auditLogHelper;

	@Autowired
	BusinessObjectService businessObjectService;
	@Autowired
	StreamService streamService;

	@Autowired
	Messages messages;

	@Autowired
	QualityNotificationValidation qualityNotificationValidation;

	@Autowired
	DestinationService destinationService;

	@Autowired
	DestinationConfigurationDao destinationConfigurationDao;

	@Autowired
	QnValidation qnValidation;
	@Autowired
	CommonFunctions commonFunctions;

	@Autowired
	private AuditLogDifference<QualityNotifications> auditLogDifference;
    @Autowired
	private AuditLogDifference<Defects> auditLogDifferenceDefects;

	private static final String QUALITY_NOTIFICATION_HANDLER = "QualityNotifcationHandler";
	private static final Logger logger = LoggerFactory.getLogger(QualityNotificationHandler.class);

	private static final String BO_TYPE = Constants.QUALITYNOTIFICATION_CODE;
	private static final boolean IS_ACTIVE = false;
	private static final String ENTITY_NAME = "QUALITYNOTIFICATION";

	@Before(event = DraftService.EVENT_DRAFT_NEW)
	public void beforeQualityNotificationDraft(DraftNewEventContext context, QualityNotifications qn) {
		qn.setStatusCode(Constants.STATUS_NEW);
		qn.setDefect(Struct.create(Defects.class));
		if (qn.getComplaintId() != null) {
			QualityNotifications oldDraftQualityNotification = qualityNotificationService
					.getDraftQualityNotificationByComplaintID(qn.getComplaintId());
			if (oldDraftQualityNotification != null) {
				Defects oldDraftDefect = qualityNotificationService
						.getDraftDefectByQualityNotificationID(oldDraftQualityNotification.getId());
				qualityNotificationService
						.deleteDraftQualityNotificationByID(oldDraftQualityNotification.getId());
				qn.putAll(oldDraftQualityNotification);
				qn.getDefect().putAll(oldDraftDefect);
			}
		}
		beforeQualityNotificationPatch(qn);
	}

	public void beforeQualityNotificationPatch(QualityNotifications qn) {
		if (qn.getComplaintId() != null) {
			// set the master data details to the QN from the complaint
			Complaints complaintDetails = setMasterDataFromComplaints(qn);
			String complaintTypeCode = null != complaintDetails ? complaintDetails.getComplaintTypeCode() : "";
			qualityNotificationService.setConfiguredValues(qn, BO_TYPE, complaintTypeCode);

		}
	}

	@Before(event = { CdsService.EVENT_CREATE }, entity = QualityNotifications_.CDS_NAME)
	public void beforeQualityNotificationCreate(QualityNotifications qn) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_HANDLER, "beforeQualityNotificationCreate");
		beforeQualityNotificationPatch(qn);
		complaintService.validateComplaintStatus(qn.getComplaintId());
		qualityNotificationService.validateIfQNExistsForComplaint(qn.getComplaintId());
		qualityNotificationService.validateQNDetails(qn);
		qualityNotificationValidation.validateQNFreeTextFields(qn);
		messages.throwIfError();
	}

	@On(event = CdsService.EVENT_CREATE, entity = QualityNotifications_.CDS_NAME)
	public void onQualityNotificationCreate(QualityNotifications qn) {
		logger.info("Inside On create event of QN");
		qualityNotificationService.createQualityNotification(qn);
		String backendStatusCode = qn.getStatusCode();
		updateBusinessObjectIdInBusinessObjects(qn, BO_TYPE);
		setBOAndComplaintStatusOnCreateUpdateQn(qn, backendStatusCode, IS_ACTIVE);
	}

	public void updateBusinessObjectIdInBusinessObjects(QualityNotifications qn, String boType) {
		// update business object with the QN ID
		businessObjectService.updateBusinessObjects(qn, boType);
	}

	@After(event = { CdsService.EVENT_CREATE }, entity = QualityNotifications_.CDS_NAME)
	public void afterQualityNotificationCreate(QualityNotifications qn) {
		logUpsert(qn);
		logUpsertDefect(qn);

	}

	@After(event = { CdsService.EVENT_CREATE }, entity = QualityNotifications_.CDS_NAME)
	public void afterQualityNotificationCreateUpdate(QualityNotifications qn) {
		// Update Stream status
		updateStreamStatus(qn, BO_TYPE, IS_ACTIVE);
		logger.info("Creation of Quality Notification Business Object is completed successfully.");
		qualityNotificationService.checkIfDuplicateQNExistsAndDelete(qn.getIdentifier());
	}

	@After(event = CdsService.EVENT_UPDATE, entity = QualityNotifications_.CDS_NAME)
	public void afterQualityNotificationUpdate(QualityNotifications qn) {
		// Update Stream status
		updateStreamStatus(qn, BO_TYPE, IS_ACTIVE);
		logger.info("Creation of Quality Notification Business Object is completed successfully.");
		logUpsert(qn);
		logUpsertDefect(qn);
	}

	public void updateStreamStatus(QualityNotifications qn, String boType, boolean isActive) {
		logger.info("QN ID : {} ", qn.getId());
		logger.info("isActive : {} ", isActive);
		streamService.updateStreamStatus(qn.getId(), boType, isActive);
	}

	@Before(event = { CdsService.EVENT_UPDATE }, entity = QualityNotifications_.CDS_NAME)
	public void beforeQualityNotificationUpdate(QualityNotifications qn) {
		qualityNotificationService.validateQNExistsAndFieldControl(qn);
		qualityNotificationValidation.validateQNFreeTextFields(qn);
		messages.throwIfError();
		setOldAuditData(qn);
		setOldDefectAuditData(qn);
	}

	/**
	 * 1. Get the corresponding CMH cockpit status of QN for the backend status and
	 * insert in BusinessObjectStatus entity 2. Get the current BO status and set it
	 * to the status attribute to display the latest BO status in UI
	 */
	public void setBOAndComplaintStatusOnCreateUpdateQn(QualityNotifications qn, String backendStatusCode,
			boolean isActive) {
		List<BusinessObjectStatuses> businessObjectStatusList = qn.getBusinessObjectStatuses();
		// BO Status will be inserted only when QN is created manually
		if (null == businessObjectStatusList || businessObjectStatusList.isEmpty()) {
			logger.info("Before inserting BO status {} :: ", backendStatusCode);
			businessObjectService.setBusinessObjectStatus(BO_TYPE, backendStatusCode, qn.getId(), isActive);
			String currentStatus = businessObjectService.getCurrentBOStatus(qn.getId());
			qn.setStatusCode(currentStatus);
			logger.info("Quality Notification created successfully with current status {}", currentStatus);
		}
		// Update complaint status to INPR
		complaintService.updateComplaintStatus(qn.getComplaintId(), Constants.COMPLAINT_IN_PROGRESS);
		logger.info("Complaint status updated to INPR!!");
	}

	@Before(event = CdsService.EVENT_READ, entity = Addresses_.CDS_NAME)
	public void beforeQualityNotificationReadAddress(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_HANDLER, "beforeQualityNotificationReadAddress");
		logger.info("Before Read of Address in Quality Notification");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_HANDLER, "beforeQualityNotificationReadAddress");
	}

	@Before(event = CdsService.EVENT_READ, entity = BusinessPartners_.CDS_NAME)
	public void beforeQualityNotificationReadBusinessPartner(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, QUALITY_NOTIFICATION_HANDLER,
				"beforeQualityNotificationReadBusinessPartner");
		logger.info("Before Read of Business Partners in Quality Notification");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, QUALITY_NOTIFICATION_HANDLER,
				"beforeQualityNotificationReadBusinessPartner");
	}

	@After(event = CdsService.EVENT_READ, entity = QualityNotifications_.CDS_NAME)
	public void afterQualityNotificationReadUpdate(CdsReadEventContext context, List<QualityNotifications> data) {
		UserInfo user = context.getUserInfo();
		String companyCode = "";
		boolean hasQualityNotificationUpdateRole = user.hasRole("QualityNotification.Update");
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
		for (QualityNotifications qn : data) {
			setFieldControl(qn);
			qn.setNumber(qn.getIdentifier());
			qn.setIdentifier(qn.getIdentifier() != null ? qn.getIdentifier().replaceFirst("^0+(?!$)", "") : "");
			qn.setIsUpdateRestricted(!hasQualityNotificationUpdateRole);
			if (null != qn.getCompanyId()) {
				companyCode = qn.getCompanyId();
			}
			QualityNotifications qualityNotificationDetailsFromDB = qualityNotificationService
					.getStatusAndCompanyCode(qn.getId());
			if (qualityNotificationDetailsFromDB != null
					&& StringUtils.isNotBlank(qualityNotificationDetailsFromDB.getStatusCode())) {
				if (!qualityNotificationDetailsFromDB.getStatusCode().equalsIgnoreCase(Constants.STATUS_NEW)) {
					qn.setIsQualityNotificationFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
					qn.setIsQualityNotificationFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
				}
				if (qualityNotificationDetailsFromDB.getStatusCode().equalsIgnoreCase(Constants.QN_STATUS_CLOSED)
						&& Boolean.FALSE.equals(qn.getIsUpdateRestricted())) {
					qn.setIsUpdateRestricted(hasQualityNotificationUpdateRole);
				}
				companyCode = qualityNotificationDetailsFromDB.getCompanyId();
			}

			/* START of Set Navigation URL */
			setNavigationURL(companyCode, readAllDestinations, qn);
			/* END of Set Navigation URL */
		}
	}

	public void setFieldControl(QualityNotifications qn) {
		if (null != qn.getIsActiveEntity() && null != qn.getHasActiveEntity()) {
			logger.info("IsActiveEntity True! : ");
			if (Boolean.TRUE.equals(qn.getIsActiveEntity()) && Boolean.FALSE.equals(qn.getHasActiveEntity())) {
				qn.setIsQualityNotificationFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
				qn.setIsQualityNotificationFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
			} else {
				logger.info("Else condition : ");
				qn.setIsQualityNotificationFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);

			}
		}
	}

	public void setNavigationURL(String companyCode, Iterator<ScpCfDestination> readAllDestinations,
			QualityNotifications qn) {
		Result result = destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(companyCode,
				Constants.QUALITYNOTIFICATION_CODE);
		Object destinationObj = result.first().isPresent() ? result.list().get(0).get("navigationDestination") : null;
		String destination = null != destinationObj ? destinationObj.toString() : "";
		logger.info("Navigation destination is : {} ", destination);
		while (readAllDestinations.hasNext()) {
			ScpCfDestination getDestination = readAllDestinations.next();
			if (null != destination && getDestination.getName().equals(destination) && getDestination.isHttp()) {
				logger.info("Navigation destination name is : {} ", getDestination.getName());
				qn.setSNavigation(getDestination.getName());
			}
		}
	}

	/**
	 * Set the master data details from the complaint
	 * 
	 * @param qn
	 */
	public Complaints setMasterDataFromComplaints(QualityNotifications qn) {
		String complaintID = qn.getComplaintId();
		Complaints complaint = complaintService.getMasterDataFromComplaints(complaintID);
		if (null != complaint) {
			qn.setMaterialId(complaint.getMaterialId());
			qn.setPlantId(complaint.getPlantId());
			qn.setSupplierId(complaint.getSupplierId());
			qn.setPurchasingOrganizationId(complaint.getPurchasingOrganizationId());
			qn.setQuantity(complaint.getQuantity());
			qn.setUnit(complaint.getUnitCode());
			qn.setCompanyId(complaint.getCompanyCodeId());
            qn.setReferenceNumber(complaint.getReferenceNumber());	
		}

		return complaint;
	}

	public void setOldAuditData(QualityNotifications qn) {
		QualityNotifications oldData = qualityNotificationService.getQualityNotificationDetails(qn.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void logUpsert(QualityNotifications qn) {
		QualityNotifications newData = qualityNotificationService.getQualityNotificationDetails(qn.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

	public void setOldDefectAuditData(QualityNotifications qn) {
		Defects oldData = qualityNotificationService.getDefectBasedOnQN(qn.getId());
		auditLogDifferenceDefects.setOldData(oldData);
	}

	public void logUpsertDefect(QualityNotifications qn) {
		Defects newData = qualityNotificationService.getDefectBasedOnQN(qn.getId());
		List<ObjectDiff> diffList = auditLogDifferenceDefects.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				"DEFECT", newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

}