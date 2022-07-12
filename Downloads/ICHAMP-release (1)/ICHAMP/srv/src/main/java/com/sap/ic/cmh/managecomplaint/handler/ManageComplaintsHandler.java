package com.sap.ic.cmh.managecomplaint.handler;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.cds.Row;
import java.util.Map;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.handler.ComplaintHandler;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.BTPUsers;
import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managecomplaintservice.Complaints_;
import cds.gen.managecomplaintservice.CostCollectors;
import cds.gen.managecomplaintservice.CostCollectors_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.qualitynotificationservice.QualityNotifications;

@Component
@ServiceName("ManageComplaintService")
public class ManageComplaintsHandler implements EventHandler {

	@Autowired
	ComplaintHandler complaintHandler;
	@Autowired
	CommonFunctions commonFunctions;
	@Autowired
	ComplaintService complaintService;

	@Autowired
	ComplaintValidation complaintValidation;

	@Autowired
	CostCollectorService costCollectorService;

	@Autowired
	ConfigurationService configurationService;

	@Autowired
	HttpService httpService;

	@Autowired
	Messages messages;
	@Autowired
	ComplaintsDao complaintsDao;
	@Autowired
	QualityNotificationService qualityNotificationService;

	@Autowired
	ClaimService claimService;

	@Autowired
	private AuditLogHelper<cds.gen.complaintservice.Complaints> auditLogHelper;

	private static final String ENTITY_NAME = "MANAGE_COMPLAINTS";
	private static final Logger logger = LoggerFactory.getLogger(ManageComplaintsHandler.class);
	private static final String MANAGE_COMPLAINT_HANDLER = "ManageComplaintsHandler";

	/**
	 * This method is used to call the BEFORE-CREATE event of draft Complaint
	 * Handler Validate Master data deletion and set company code and currency
	 * derived from plant
	 *
	 * @param manageComplaints
	 */
	@Before(event = CdsService.EVENT_CREATE, entity = Complaints_.CDS_NAME)
	public void beforeManageComplaintCreate(Complaints manageComplaints) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "beforeManageComplaintCreate");
		MasterData masterdata = new MasterData();
		setMasterDataDetails(manageComplaints, masterdata);
		cds.gen.complaintservice.Complaints complaints = commonFunctions
				.convertManageComplaintsToComplaints(manageComplaints);
		synchronized (this) {
			complaints.setId(UUID.randomUUID().toString());
		}
		complaintHandler.beforeComplaintCreate(complaints);
		manageComplaints.setId(complaints.getId());
		manageComplaints.setCompanyCodeId(complaints.getCompanyCodeId());
		manageComplaints.setCurrencyCode(complaints.getCurrencyCode());
		manageComplaints.setLaborUnitCode(complaints.getLaborUnitCode());
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER, "beforeManageComplaintCreate");
	}

	/**
	 * This method is used to call the ON-CREATE event of draft Complaint Handler
	 * Set default complaint type, status, creation type, identifier,streams
	 *
	 * @param manageComplaints
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Complaints_.CDS_NAME)
	public void onManageComplaintsCreate(Complaints manageComplaints) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "onManageComplaintsCreate");
		cds.gen.complaintservice.Complaints complaints = commonFunctions
				.convertManageComplaintsToComplaints(manageComplaints);
		logger.info("Complaint's Creation Type is ::: {} ", manageComplaints.getCreationType());
		complaintHandler.onComplaintCreate(complaints);
		manageComplaints.setId(complaints.getId());
		manageComplaints.setCreationType(complaints.getCreationType());
		manageComplaints.setComplaintTypeCode(complaints.getComplaintTypeCode());
		manageComplaints.setComplaintStatusCode(complaints.getComplaintStatusCode());
		manageComplaints.setIdentifier(complaints.getIdentifier());
		manageComplaints.setStreams(complaints.getStreams());
		manageComplaints.setTotalLaborHour(new BigDecimal(0));
		manageComplaints.setTotalSubLetCost(new BigDecimal(0));
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER, "onManageComplaintsCreate");
	}

	/**
	 * This method is used to call the BEFORE-CREATE event of draft Complaint
	 * Handler Validate Field control and validate free text fields
	 *
	 * @param {link Complaints} manageComplaints
	 */
	@Before(event = CdsService.EVENT_UPDATE, entity = Complaints_.CDS_NAME)
	public void beforeComplaintUpdate(Complaints manageComplaints) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "beforeComplaintUpdate");
		MasterData masterdata = new MasterData();
		setMasterDataDetails(manageComplaints, masterdata);
		if (null != manageComplaints.getCreationType()
				&& manageComplaints.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_MANUAL)) {
			setPurchaseOrganization(manageComplaints);
		}
		cds.gen.complaintservice.Complaints complaints = commonFunctions
				.convertManageComplaintsToComplaints(manageComplaints);
		complaintHandler.beforeComplaintUpdate(complaints);
		if (Constants.COMPLAINT_CREATED_STATUS.equalsIgnoreCase(manageComplaints.getComplaintStatusCode())
				|| (null != manageComplaints.getCreationType()
				&& manageComplaints.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_AUTOMATIC))) {
			manageComplaints.setCompanyCodeId(complaints.getCompanyCodeId());
			manageComplaints.setCurrencyCode(complaints.getCurrencyCode());
		}
		setOldAuditData(manageComplaints);
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER, "beforeComplaintUpdate");
	}

	@After(event = {CdsService.EVENT_CREATE}, entity = Complaints_.CDS_NAME)
	public void afterManageComplaintsCreate(Complaints manageComplaints) {
		logUpsert(Action.CREATE,manageComplaints);
	}

	private void setPurchaseOrganization(Complaints manageComplaints) {
		Result complaintDetailsResult = complaintsDao.getComplaintDetails(manageComplaints.getId());
		Complaints complaintDetails = complaintDetailsResult.first().isPresent()
				? complaintDetailsResult.listOf(Complaints.class).get(0)
				: null;
		if (null != complaintDetails && null == manageComplaints.getPurchasingOrganizationId()) {
			manageComplaints.setPurchasingOrganizationId(complaintDetails.getPurchasingOrganizationId());
		}
	}

	/**
	 * This method is used to call the AFTER-UPDATE event of draft Complaint Handler
	 *
	 * @param manageComplaints
	 */
	@After(event = CdsService.EVENT_UPDATE, entity = Complaints_.CDS_NAME)
	public void afterManageComplaintUpdate(Complaints manageComplaints) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "afterManageComplaintUpdate");
		cds.gen.complaintservice.Complaints complaints = commonFunctions
				.convertManageComplaintsToComplaints(manageComplaints);

		complaintHandler.afterComplaintUpdate(complaints);
		manageComplaints.setIdentifier(complaints.getIdentifier());
		manageComplaints.setCreationType(complaints.getCreationType());
		manageComplaints.setQuantity(complaints.getQuantity());
		manageComplaints.setNote(complaints.getNote());
		manageComplaints.setReferenceNumber(complaints.getReferenceNumber());
		manageComplaints.setDescription(complaints.getDescription());
		logUpsert(Action.UPDATE,manageComplaints);
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER, "afterManageComplaintUpdate");	}

	@Before(event = CdsService.EVENT_CREATE, entity = CostCollectors_.CDS_NAME)
	public void beforeCostCollectorCreate(CostCollectors manageCostCollectors) {
		String complaintId = StringUtils.isNotBlank(manageCostCollectors.getComplaintCode())
				? getComplaintIdBasedOnCode(manageCostCollectors.getComplaintCode())
				: "";
		if(StringUtils.isNotBlank(complaintId)) {
			manageCostCollectors.setParentId(complaintId);
			checkIfClaimIsCreated(complaintId);
			messages.throwIfError();
		}else {
			throw new ServiceException(MessageKeys.COMPLAINT_ASSOCIATION_TO_COST_COLLECTOR);
		}
	}

	@On(event = CdsService.EVENT_CREATE, entity = CostCollectors_.CDS_NAME)
	public void onCostCollectorCreate(CostCollectors manageCostCollectors) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "onCostCollectorCreate");
		if (manageCostCollectors.getTotalCost() != null) {
			cds.gen.complaintservice.Complaints complaint = complaintService
					.getComplaintDetails(manageCostCollectors.getParentId());
			cds.gen.costcollectorservice.CostCollectors costCollectors = commonFunctions
					.convertManageCostCollectorToCostCollector(manageCostCollectors);
			BigDecimal convertedCost = costCollectorService.calculateExchangeRateAndConvertCost(costCollectors,
					complaint);
			if (convertedCost != null) {
				logger.info("***********The total cost is ::: {}", convertedCost);
				complaintService.updateComplaintWithCost(manageCostCollectors.getParentId(), convertedCost);
			} else {
				messages.error(MessageKeys.UNSUCESSFUL_INSERTION);
			}
			messages.throwIfError();
		}
		if (manageCostCollectors.getQuantity() != null) {
			BigDecimal quantity = new BigDecimal(manageCostCollectors.getQuantity().toString());
			complaintService.updateComplaintWithQuantity(manageCostCollectors.getParentId(), quantity);
		}
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER, "onCostCollectorCreate");
	}

	private void checkIfClaimIsCreated(String complaintId) {
		QualityNotifications qn = qualityNotificationService
				.getQualityNotificationDetailsByComplaintId(complaintId);
		Claims claim = claimService.getClaim(complaintId);
		if(qn==null) {
			messages.error(MessageKeys.COST_COLLECTOR_CREATE_ERROR);
		}else if(null!=claim) {
			messages.error(MessageKeys.COST_COLLECTOR_NOT_UPDATABLE);
		}

	}

	private String getComplaintIdBasedOnCode(String complaintCode) {
		Result comp = complaintsDao.getComplaintBasedOnCode(complaintCode);
		Optional<Row> complaintFirst = comp.first();
		return complaintFirst.isPresent() ? complaintFirst.get().get(Complaints.ID).toString() : "";
	}

	private void setMasterDataDetails(Complaints manageComplaints, MasterData masterdata) {
		configurationService.validateMaterial(manageComplaints.getMaterialCode(), masterdata);
		manageComplaints.setMaterialId(null != masterdata.getMaterial() ? masterdata.getMaterial().getId() : null);
		if (manageComplaints.getUnitCode() == null && manageComplaints.getMaterialId() != null
				&& masterdata.getMaterial().getBaseUnitOfMeasureCode() != null) {
			manageComplaints.setUnitCode(masterdata.getMaterial().getBaseUnitOfMeasureCode());
		}
		configurationService.validateSupplier(manageComplaints.getSupplierCode(), masterdata);
		manageComplaints.setSupplierId(null != masterdata.getSupplier() ? masterdata.getSupplier().getId() : null);
		configurationService.validatePlant(manageComplaints.getPlantCode(), masterdata);
		manageComplaints.setPlantId(null != masterdata.getPlants() ? masterdata.getPlants().getId() : null);
		configurationService.validatePurchaseOrganisation(manageComplaints.getPurchaseOrganisationCode(), masterdata);
		manageComplaints.setPurchasingOrganizationId(
				null != masterdata.getPurchaseOrg() ? masterdata.getPurchaseOrg().getId() : null);
		if (null == manageComplaints.getCreationType() && manageComplaints.getPersonResponsibleCode() != null
				&& complaintValidation.validateResponsiblePerson(manageComplaints.getPersonResponsibleCode())) {
			manageComplaints.setPersonResponsibleId(manageComplaints.getPersonResponsibleCode());
		}
		BusinessPartners businessPartner = configurationService
				.validateSupplierContactPerson(manageComplaints.getContactPersonCode());
		if (businessPartner != null) {
			manageComplaints.setContactPersonId(businessPartner.getId());
		}
		messages.throwIfError();
	}

	@After(event = CdsService.EVENT_READ, entity = Complaints_.CDS_NAME)
	public void afterComplaintRead(List<Complaints> data) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER, "afterComplaintRead");
		int count = 0;
		List<BTPUsers> responsiblePersonList = complaintService.getAllResponsiblePerson();
		HashMap<Integer, String> map = new HashMap<>();
		if(!CollectionUtils.isEmpty(responsiblePersonList)) {
			for (BTPUsers btpuser : responsiblePersonList) {
				map.put(count, btpuser.getPersonResponsibleId());
				count++;
			}
			for (Complaints comp : data) {
				String responsiblePerson = comp.getPersonResponsibleId();
				if (comp.getComplaintStatusCode().equals(Constants.COMPLAINT_CLOSED)
						&& !map.containsValue(responsiblePerson) && StringUtils.isNotEmpty(comp.getPersonResponsibleId())) {
					comp.setPersonResponsibleId("******");
				}
			}
		}

	}
	/**
	 * calling from Before Update for set old data
	 * values
	 *
	 * @param {@link Complaints} complaint
	 *
	 * @public
	 */
	public void setOldAuditData(Complaints complaints) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER,"setOldAuditData");
		cds.gen.complaintservice.Complaints oldData = complaintService.getComplaintDetails(complaints.getId());
		auditLogHelper.setOldData(oldData);
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER,"setOldAuditData");
	}
	/**
	 * calling from After create, Update for audit log data
	 * values
	 *
	 * @param {@link Complaints} newData
	 *@param {@link Action} action
	 * @public
	 */
	public void logUpsert(Action action, Complaints newData) {
		LoggerHelper.logMethodEntry(logger, MANAGE_COMPLAINT_HANDLER,"logUpsert");
		auditLogHelper.logConfigChange(Complaints_.CDS_NAME, action, newData);
		LoggerHelper.logMethodExit(logger, MANAGE_COMPLAINT_HANDLER,"logUpsert");
	}

}
