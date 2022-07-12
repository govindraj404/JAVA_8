package com.sap.ic.cmh.supplierissueprocess.handler;

import java.util.List;
import java.util.Map;

import com.sap.ic.cmh.supplierissueprocess.validations.EightDValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.apache.commons.lang3.StringUtils;
import com.sap.cds.services.cds.CdsCreateEventContext;
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
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.persistency.QualityNotificationDao;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses_;
import cds.gen.supplierissueprocessservice.Addresses_;
import cds.gen.supplierissueprocessservice.BusinessPartners_;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;

@Component
@ServiceName("SupplierIssueProcessService")
public class EightDHandler implements EventHandler {

	@Autowired
	StreamService streamService;

	@Autowired
	EightDService eightDService;

	@Autowired
	private AuditLogHelper auditLogHelper;

	@Autowired
	BusinessObjectService businessObjectService;

	@Autowired
	ComplaintService complaintService;

	@Autowired
	HttpService httpService;

	@Autowired
	QualityNotificationService qnService;

	@Autowired
	QualityNotificationDao qnDao;

	@Autowired
	Messages messages;

	@Autowired
	EightDValidation eightDValidation;

	@Autowired
	CommonFunctions commonFunctions;

	@Autowired
	private AuditLogDifference<Supplier8DProcesses> auditLogDifference;

	private static final String ENTITY_NAME = "SUPPLIER 8D";

	private static final Logger logger = LoggerFactory.getLogger(EightDHandler.class);
	private static final String SUPPLIER_8D_HANDLER = "EightDHandler";
	private static final String ON_SUPPLIER_8D_CREATE = "onSupplier8DCreate";
	private static final String AFTER_SUPPLIER_8D_CREATE = "afterSupplier8DCreate";
	private static final String REMOVE_LEADING_ZERO = "^0+(?!$)";
	private static final String BO_TYPE = Constants.SUPPLIER_EIGHTD_CODE;
	private static final boolean IS_ACTIVE = false;

	@Before(event = DraftService.EVENT_DRAFT_NEW)
	public void beforeQualityNotificationDraft(DraftNewEventContext context, Supplier8DProcesses eightD) {
		eightD.setStatusCode(Constants.STATUS_NEW);
		if (eightD.getComplaintId() != null) {
			Supplier8DProcesses oldDraftEightD = eightDService.getDraftEightDByComplaintID(eightD.getComplaintId());
			if (oldDraftEightD != null) {
				eightD.putAll(oldDraftEightD);
				eightDService.deleteDraftEightDByID(oldDraftEightD.getId());
			}
		}
		beforeSupplier8DPatch(eightD);
	}

	/**
	 * Get the master data from complaints, fetch configured business object
	 * attributes for Supplier 8D and get the defect details
	 * 
	 * @param eightD
	 */
	// @Before(event = DraftService.EVENT_DRAFT_PATCH)
	public void beforeSupplier8DPatch(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DPatch");
		if (eightD.getComplaintId() != null) {
			Complaints complaintDetails = setMasterDataFromComplaints(eightD);
			String complaintTypeCode = null != complaintDetails ? complaintDetails.getComplaintTypeCode() : "";
			eightDService.setConfiguredValues(eightD, BO_TYPE, complaintTypeCode);
			QualityNotifications qn = qnDao.fetchQNForSupplier8d(eightD.getComplaintId());
			if (qn != null) {
				Defects defect = qn.getDefect();
				eightD.setDefectId(defect.getId());
			}
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DPatch");
	}

	/**
	 * Validate if Supplier 8D is already created for the complaint and validate the
	 * mandatory fields and field editability
	 * 
	 * @param eightD
	 */
	@Before(event = { CdsService.EVENT_CREATE }, entity = Supplier8DProcesses_.CDS_NAME)
	public void beforeSupplier8DCreate(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DCreate");
		beforeSupplier8DPatch(eightD);
		complaintService.validateComplaintStatus(eightD.getComplaintId());
		eightDService.validateIf8DExistsForComplaint(eightD.getComplaintId());
		eightDService.validate8DDetails(eightD);
		messages.throwIfError();
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DCreate");

	}

	/**
	 * validate the mandatory fields and field editability
	 * 
	 * @param eightD
	 */
	@Before(event = { CdsService.EVENT_UPDATE }, entity = Supplier8DProcesses_.CDS_NAME)
	public void beforeSupplier8DUpdate(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DUpdate");
		eightDService.validateIf8DExists(eightD);
		messages.throwIfError();
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DUpdate");
		setOldAuditData(eightD);
	}

	/**
	 * Create supplier 8D in the target system and update the business object id for
	 * the complaint Find the cockpit status code for the target system status and
	 * insert the business object status
	 * 
	 * @param eightD
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Supplier8DProcesses_.CDS_NAME)
	public void onSupplier8DCreate(Supplier8DProcesses eightD, CdsCreateEventContext context) {
		UserInfo userInfo = context.getUserInfo();
		String userName = userInfo.getName(); // Returns the verified user name of the request's authenticated user.
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, ON_SUPPLIER_8D_CREATE);
		logger.info("Inside On create event of EightD");
		eightDService.create8D(eightD, userName);

		String backendStatusCode = eightD.getStatusCode();
		businessObjectService.updateBusinessObjects(eightD, BO_TYPE);
		setBusinessObjectStatus(eightD, backendStatusCode, IS_ACTIVE);
		logger.info("Supplier EightD created successfully {}", eightD.getIdentifier());
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, ON_SUPPLIER_8D_CREATE);
	}

	/**
	 * insert the business object status based on the backend status code
	 * 
	 * @param eightD
	 * @param backendStatusCode
	 * @param isActive
	 */
	public void setBusinessObjectStatus(Supplier8DProcesses eightD, String backendStatusCode, boolean isActive) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, ON_SUPPLIER_8D_CREATE);
		businessObjectService.setBusinessObjectStatus(BO_TYPE, backendStatusCode, eightD.getId(), isActive);
		setCurrentBOStatus(eightD);
		logger.info("Supplier EightD created successfully {}", eightD.getIdentifier());
         // Update complaint status to INPR
		complaintService.updateComplaintStatus(eightD.getComplaintId(), Constants.COMPLAINT_IN_PROGRESS);
		logger.info("Complaint status updated to INPR!!");
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, ON_SUPPLIER_8D_CREATE);
	}

	public void setCurrentBOStatus(Supplier8DProcesses eightD) {
		String currentStatus = businessObjectService.getCurrentBOStatus(eightD.getId());
		eightD.setStatusCode(currentStatus);
	}

	/**
	 * Update stream status after creating and updating Supplier 8D
	 * 
	 * @param eightD
	 */
	@After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = Supplier8DProcesses_.CDS_NAME)
	public void afterSupplier8DCreate(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, AFTER_SUPPLIER_8D_CREATE);
		eightD.setNumber(eightD.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, ""));
		updateStreamStatus(eightD, BO_TYPE, IS_ACTIVE);
		logger.info("EightD Business Object is Created Successfully.");
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, AFTER_SUPPLIER_8D_CREATE);
		logUpsert(eightD);
	}

	/**
	 * Update stream status
	 * 
	 * @param eightD
	 * @param boType
	 * @param isActive
	 */
	public void updateStreamStatus(Supplier8DProcesses eightD, String boType, boolean isActive) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, AFTER_SUPPLIER_8D_CREATE);
		streamService.updateStreamStatus(eightD.getId(), boType, isActive);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, AFTER_SUPPLIER_8D_CREATE);
	}

	@Before(event = CdsService.EVENT_READ, entity = Addresses_.CDS_NAME)
	public void beforeSupplier8DReadAddress(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DReadAddress");
		logger.info("Before Read of Address in Supplier 8D");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DReadAddress");
	}

	@Before(event = CdsService.EVENT_READ, entity = BusinessPartners_.CDS_NAME)
	public void beforeSupplier8DReadBusinessPartner(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DReadBusinessPartner");
		logger.info("Before Read of Business Partners in Supplier 8D");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "beforeSupplier8DReadBusinessPartner");
	}

	@After(event = CdsService.EVENT_READ, entity = Supplier8DProcesses_.CDS_NAME)
	public void afterSupplier8DReadUpdate(CdsReadEventContext context, List<Supplier8DProcesses> data) {
		UserInfo user = context.getUserInfo();
		boolean hasEightDUpdateRole = user.hasRole("SupplierIssueProcess.Update");
		for (Supplier8DProcesses eightD : data) {
			eightD.setNumber(eightD.getIdentifier());
			setQualityNotificationNumber(eightD);
			eightD.setIdentifier(
					eightD.getIdentifier() != null ? eightD.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "") : "");
			eightD.setIsUpdateRestricted(!hasEightDUpdateRole);
			Supplier8DProcesses eightDDetailsFromDB = eightDService.getEightDDetails(eightD.getId());
			if (eightDDetailsFromDB != null && StringUtils.isNotBlank(eightDDetailsFromDB.getStatusCode())) {
				if (!eightDDetailsFromDB.getStatusCode().equalsIgnoreCase(Constants.STATUS_NEW)) {
					eightD.setIsSupplierFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
				}
				if (eightDDetailsFromDB.getStatusCode().equalsIgnoreCase(Constants.SUPPLIER_ISSUE_PROCESS_STATUS_CLOSED)
						&& Boolean.FALSE.equals(eightD.getIsUpdateRestricted())) {
					eightD.setIsUpdateRestricted(hasEightDUpdateRole);
				}

			}

		}
	}

	/**
	 * Audit Logging
	 * 
	 * @param data
	 */

	/**
	 * Set the master data details from the complaint
	 *
	 * @param eightD
	 */
	public Complaints setMasterDataFromComplaints(Supplier8DProcesses eightD) {
		LoggerHelper.logMethodEntry(logger, SUPPLIER_8D_HANDLER, "setMasterDataFromComplaints");
		String complaintID = eightD.getComplaintId();
		Complaints complaint = complaintService.getMasterDataFromComplaints(complaintID);
		if (null != complaint) {
			eightD.setMaterialId(complaint.getMaterialId());
			eightD.setPlantId(complaint.getPlantId());
			eightD.setSupplierId(complaint.getSupplierId());
			eightD.setPurchasingOrganizationId(complaint.getPurchasingOrganizationId());
			eightD.setQuantity(complaint.getQuantity());
			eightD.setUnit(complaint.getUnitCode());
			eightD.setCompanyId(complaint.getCompanyCodeId());
		}
		LoggerHelper.logMethodExit(logger, SUPPLIER_8D_HANDLER, "setMasterDataFromComplaints");
		return complaint;
	}

	public void setOldAuditData(Supplier8DProcesses eightD) {
		Supplier8DProcesses oldData = eightDService.getEightDBasedOnId(eightD.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void logUpsert(Supplier8DProcesses eightD) {
		Supplier8DProcesses newData = eightDService.getEightDBasedOnId(eightD.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

	private void setQualityNotificationNumber(Supplier8DProcesses eightD) {
		if (eightD.getDefect() != null && eightD.getDefect().getParent() != null
				&& eightD.getDefect().getParent().getIdentifier() != null) {
			eightD.getDefect().getParent().setIdentifier(
					eightD.getDefect().getParent().getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, ""));
		}
	}
}