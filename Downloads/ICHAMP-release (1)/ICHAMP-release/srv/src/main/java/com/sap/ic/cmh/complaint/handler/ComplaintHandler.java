package com.sap.ic.cmh.complaint.handler;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.*;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.sap.cds.Result;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.complaint.validation.StreamValidation;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import cds.gen.complaintservice.QualityNotification;
import cds.gen.complaintservice.QualityNotification_;
import cds.gen.complaintservice.Claim;
import cds.gen.complaintservice.Claim_;
import cds.gen.complaintservice.Supplier8DProcess;
import cds.gen.complaintservice.Supplier8DProcess_;
import cds.gen.complaintservice.ReturnPurchaseOrder;
import cds.gen.complaintservice.ReturnPurchaseOrder_;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import java.util.Iterator;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.network.service.DestinationService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
@ServiceName("ComplaintService")
public class ComplaintHandler implements EventHandler {

	@Autowired
	ComplaintValidation validator;

	@Autowired
	private AuditLogHelper<Complaints> auditLogHelper;

	@Autowired
	ComplaintService complaintService;

	@Autowired
	StreamService streamService;

	@Autowired
	MasterDataService masterDataService;

	@Autowired
	StreamValidation streamValidator;
	@Autowired
	Messages messages;

	@Autowired
	QualityNotificationService qualityNotificationService;

	@Autowired
	DestinationService destinationService;

	@Autowired
	DestinationConfigurationDao destinationConfigurationDao;

	@Autowired
	ClaimService claimService;
	@Autowired
	CommonFunctions commonFunctions;

	private static final String ENTITY_NAME = "Complaint";


	public static final Logger logger = LoggerHelper.getLogger(ComplaintHandler.class);
	private static final String COMPLAINT_HANDLER = "ComplaintHandler";
	private static final String REMOVE_LEADING_ZERO = "^0+(?!$)";

	/**
	 * Prefill company code based on plant and currency based on company code
	 *
	 * @param complaint
	 */
	@Before(event = DraftService.EVENT_DRAFT_PATCH, entity = Complaints_.CDS_NAME)
	public void beforeComplaintPatch(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintPatch");
		if (complaint.getPlantId() != null) {
			String companyCodeIDId = masterDataService.getCompanyCodeBasedOnPlants(complaint.getPlantId())
					.getCompanyCodeIDId();
			complaint.setCompanyCodeId(companyCodeIDId);
		}
		if (complaint.getCompanyCodeId() != null) {
			complaint.setCurrencyCode(
					masterDataService.getCurrencyBasedOnCompanyCodes(complaint.getCompanyCodeId()).getCurrencyCode());
		}
		if (complaint.getLaborUnitCode() == null) {
			complaint.setLaborUnitCode(Constants.DEFAULT_LABOR_UNIT);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintPatch");
	}

	/**
	 * Set the initial status of complaint
	 *
	 * @param complaint
	 */
	@Before(event = DraftService.EVENT_DRAFT_NEW, entity = Complaints_.CDS_NAME)
	public void beforeComplaintDraftCreation(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintDraftCreation");
		complaint.setComplaintStatusCode(Constants.STATUS_NEW);
		complaint.setIsHideDiscardComplaint(true);
		complaint.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
		complaint.setTotalLaborHour(new BigDecimal(0));
		complaint.setTotalSubLetCost(new BigDecimal(0));
		complaint.setLaborUnitCode(Constants.DEFAULT_LABOR_UNIT);
		logger.info("Complaint data set Successfully");
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintDraftCreation");
	}

	/**
	 * Set company code and currency before creation of a complaint
	 *
	 * @param complaint
	 */
	@Before(event = CdsService.EVENT_CREATE, entity = Complaints_.CDS_NAME)
	public void beforeComplaintCreate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintCreate");
		beforeComplaintPatch(complaint);
		// Based on creation type, validate for plant
		if (null == complaint.getCreationType()
				|| !complaint.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_AUTOMATIC)) {
			validator.validatePlant(complaint.getPlantId());
		}
		validator.validateComplaintBeforeCreate(complaint);
		validator.validateComplaintFreeTextFields(complaint);
		messages.throwIfError();
		logger.info("Complaint data set Successfully");
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintCreate");
	}

	/**
	 * Set default complaint type, status and identifier on creation of a complaint
	 *
	 * @param complaint
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Complaints_.CDS_NAME)
	public void onComplaintCreate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "onComplaintCreate");
		if (complaint.getCreationType() == null || complaint.getCreationType().isEmpty()) {
			complaint.setCreationType(Constants.COMPLAINT_MANUAL);
		}
		setComplaintValues(complaint);
		streamService.createAllStreams(complaint.getId(), complaint.getComplaintTypeCode());
		Result getAllComplaints = complaintService.getAllComplaints();
		Integer sequenceNumber = (getAllComplaints.first().isPresent()) ? getAllComplaints.list().size() + 1 : 1;
		complaint.setIdentifier(sequenceNumber.toString());
		logger.info("Complaint Created Successfully");
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "onComplaintCreate");
	}

	@After(event = { CdsService.EVENT_CREATE }, entity = Complaints_.CDS_NAME)
	public void afterComplaintCreate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "afterComplaintCreate");
		logUpsert(Action.CREATE, complaint);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "afterComplaintCreate");

	}

	/**
	 * Validate Field control and validate free text fields
	 *
	 * @param complaint
	 */
	@Before(event = CdsService.EVENT_UPDATE, entity = Complaints_.CDS_NAME)
	public void beforeComplaintUpdate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintUpdate");
		if (Constants.COMPLAINT_CREATED_STATUS.equalsIgnoreCase(complaint.getComplaintStatusCode())
				|| (null != complaint.getCreationType()
				&& complaint.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_AUTOMATIC))) {
			beforeComplaintPatch(complaint);
		}
		if (null != complaint.getCreationType()
				&& !complaint.getCreationType().equalsIgnoreCase(Constants.COMPLAINT_AUTOMATIC)
				&& (null == complaint.getIsEventSpecificRequest()
				|| Boolean.FALSE.equals(complaint.getIsEventSpecificRequest()))) {
			complaintService.validateComplaintAttributes(complaint);
		}
		validator.validateResponsiblePerson(complaint.getPersonResponsibleId());
		validator.validateComplaintFreeTextFields(complaint);
		messages.throwIfError();
		setOldAuditData(complaint);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintUpdate");
	}

	/**
	 * Validate Master Data deletion if complaint is closed
	 *
	 * @param complaint
	 */
	@After(event = CdsService.EVENT_UPDATE, entity = Complaints_.CDS_NAME)
	public void afterComplaintUpdate(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "afterComplaintUpdate");
		Complaints complaintDetails = complaintService.getComplaintDetails(complaint.getId());
		if (complaintDetails != null && complaintDetails.getComplaintStatusCode() != null
				&& complaintDetails.getComplaintStatusCode().equals(Constants.COMPLAINT_CLOSED)) {
			complaintService.deleteBusinessPartnerWhenComplaintClosed(complaintDetails);
		}
		String complaintStatusCode = complaint.getComplaintStatusCode();
		String complaintId = complaint.getId();
		if (Constants.COMPLAINT_REVISED.equalsIgnoreCase(complaintStatusCode)) {
			if (!streamService.checkIfAllRelevantStreamsClosed(complaintId)) {
				complaintService.updateComplaintStatus(complaintId, Constants.COMPLAINT_IN_PROGRESS);
			}
		} else if (Constants.COMPLAINT_IN_PROGRESS.equalsIgnoreCase(complaintStatusCode)) {
			logger.info("Proceeding with status update for Streams");
			streamService.updateComplaintStatusBasedOnAllStreamStatus(complaintId);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "afterComplaintUpdate");
		logUpsert(Action.UPDATE, complaint);
	}

	@Before(event = { CdsService.EVENT_READ }, entity = BTPUsers_.CDS_NAME)
	public List<BTPUsers> beforeBTPUsersRead(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeBTPUsersRead");
		logger.info("*********************Before entered****************");
		ParameterInfo parameterInfo = context.getParameterInfo();
		Map<String, String> map1 = parameterInfo.getQueryParams();
		String filter = map1.get("$filter");
		String search = map1.get("$search");
		String select = map1.get("$select");
		logger.info("Select Param value {}", select);
		logger.info("***BTP users filtered*** {}  ", filter);
		List<BTPUsers> btpUsersList = new ArrayList<>();
		List<BTPUsers> list = complaintService.getAllResponsiblePerson();
		List<BTPUsers> collect;
		if (!CollectionUtils.isEmpty(list)) {
			logger.info("BTPUsers list is not empty ");
			if (null != filter) {
				String filteredParamValue = filter.split(" ")[2];
				String attribute = filter.split(" ")[0];
				String replaceAll = filteredParamValue.replace("'", "");
				logger.info("**Filtered value after replace** {} ", replaceAll);
				collect = list.stream()
						.filter(BTPUsers.PERSON_RESPONSIBLE_NUMBER.equalsIgnoreCase(attribute)
								? btp -> null != btp.getPersonResponsibleNumber()
								&& btp.getPersonResponsibleNumber().toLowerCase()
								.contains(replaceAll.toLowerCase())
								: btp -> btp.getPersonResponsibleId().toLowerCase().contains(replaceAll.toLowerCase()))
						.collect(Collectors.toList());
				logger.info("****BTP Users Filtered list in before read **** {} ", collect.size());
				btpUsersList.addAll(collect);
			} else if (null != search) {
				String[] valuesInQuotes = StringUtils.substringsBetween(search, "\"", "\"");
				logger.info(" Inside search ");
				collect = getFilteredBTPUsers(list, valuesInQuotes[0]);

				logger.info("****BTP Users Searched list in before read **** {} ", collect.size());
				btpUsersList.addAll(collect);

			} else {
				btpUsersList.addAll(list);
			}
		}

		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeBTPUsersRead");
		return btpUsersList;
	}

	@Before(event = CdsService.EVENT_READ, entity = Addresses_.CDS_NAME)
	public void beforeComplaintReadAddress(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintReadAddress");
		logger.info("Before Read of Address in Complaints");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintReadAddress");
	}

	@Before(event = CdsService.EVENT_READ, entity = BusinessPartners_.CDS_NAME)
	public void beforeComplaintReadBusinessPartner(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "beforeComplaintReadBusinessPartner");
		logger.info("Before Read of Business Partners in Complaints");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "beforeComplaintReadBusinessPartner");
	}

	public List<BTPUsers> getFilteredBTPUsers(List<BTPUsers> list, String replaceAll) {
		List<BTPUsers> collect;
		collect = list.stream().filter(
						btp -> null != btp.getPersonResponsibleNumber()
								&& btp.getPersonResponsibleNumber().toLowerCase().contains(replaceAll.toLowerCase())
								? null != btp.getPersonResponsibleNumber()
								&& btp.getPersonResponsibleNumber().toLowerCase()
								.contains(replaceAll.toLowerCase())
								: btp.getPersonResponsibleId().toLowerCase().contains(replaceAll.toLowerCase()))
				.collect(Collectors.toList());
		return collect;
	}

	/**
	 * Updating filed editability and Hiding button based on the role
	 *
	 * @param data
	 */
	@After(event = CdsService.EVENT_READ, entity = Complaints_.CDS_NAME)
	public void afterComplaintRead(CdsReadEventContext context, List<Complaints> data) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "afterComplaintRead");
		UserInfo user = context.getUserInfo();

		boolean hasComplaintUpdateRole = user.hasRole("Complaint.Update");
		boolean hasCostCollectorReadRole = user.hasRole("CostCollector.Read");
		HashMap<Integer, String> map = fetchBTPUsersList();
		for (Complaints comp : data) {
			Complaints complaint = complaintService.getComplaintDetails(comp.getId());
			comp.setIsUpdateRestricted(!hasComplaintUpdateRole);
			comp.setIsHideCostCollection(true);
			comp.setIsHideCloseComplaint(true);
			comp.setIsHideReopenComplaint(true);
			comp.setIsHideDiscardComplaint(true);
			comp.setIsShowStreams(
					hideStream(comp.getIsActiveEntity(), comp.getHasDraftEntity(), comp.getHasActiveEntity()));
			if (complaint != null && complaint.getComplaintStatusCode() != null) {
				switch (complaint.getComplaintStatusCode()) {
					case Constants.COMPLAINT_CREATED_STATUS:
						comp.setIsFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
						comp.setIsHideDiscardComplaint(!user.hasRole("Complaint.Discard"));
						break;
					case Constants.COMPLAINT_IN_PROGRESS:
						comp.setIsComplaintNew(Constants.FIELD_CONTROL_READ_ONLY);
						comp.setIsFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
						break;
					case Constants.COMPLAINT_REVISED:
						comp.setIsComplaintNew(Constants.FIELD_CONTROL_READ_ONLY);
						comp.setIsFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
						if (Boolean.TRUE.equals(comp.getIsActiveEntity())
								&& streamService.checkIfAllRelevantStreamsClosed(complaint.getId())) {
							comp.setIsHideCloseComplaint(!user.hasRole("Complaint.Close"));
						}
						break;
					case Constants.COMPLAINT_CLOSED:
						comp.setIsHideReopenComplaint(!user.hasRole("Complaint.Reopen"));
						setFlagsWhenComplaintIsClosed(hasComplaintUpdateRole, comp, complaint, map);
						break;
					case Constants.COMPLAINT_DISCARDED:
						setFlagsWhenComplaintIsClosed(hasComplaintUpdateRole, comp, complaint, map);
						break;
					default:
						break;
				}
				checkCostCollectionButtonHiding(comp, hasCostCollectorReadRole);
			}
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "afterComplaintRead");
	}

	public void checkCostCollectionButtonHiding(Complaints comp, boolean hasCostCollectorReadRole) {
		QualityNotifications qn = qualityNotificationService
				.getQualityNotificationDetailsByComplaintId(comp.getId());
		Claims claim = claimService.getClaim(comp.getId());
		if (qn != null && claim == null && hasCostCollectorReadRole) {
			comp.setIsHideCostCollection(false);
		}
	}

	private HashMap<Integer, String> fetchBTPUsersList() {
		int count = 0;
		List<BTPUsers> responsiblePersonList = complaintService.getAllResponsiblePerson();
		HashMap<Integer, String> map = new HashMap<>();
		for (BTPUsers btpuser : responsiblePersonList) {
			map.put(count, btpuser.getPersonResponsibleId());
			count++;
		}
		return map;
	}

	/**
	 * Set updateRestricted,hideAdaptStreams values Handle deletion of master data
	 * when complaint is closed
	 *
	 * @param hasComplaintUpdateRole
	 * @param comp
	 * @param complaint
	 * @param map
	 */
	public void setFlagsWhenComplaintIsClosed(boolean hasComplaintUpdateRole, Complaints comp, Complaints complaint,
											  Map<Integer, String> map) {
		if (null != comp.getIsActiveEntity() && null != comp.getHasActiveEntity()) {
			comp.setIsHideAdaptStreams(
					(comp.getIsActiveEntity() && !comp.getHasActiveEntity()) ? comp.getIsActiveEntity()
							: !comp.getIsActiveEntity());
		}
		// Mask the BTP user if the person responsible is not in the BTP User list
		String responsiblePerson = comp.getPersonResponsibleId();
		if (!map.containsValue(responsiblePerson) && StringUtils.isNotEmpty(responsiblePerson)) {
			comp.setPersonResponsibleId("******");
		}
		boolean isUpdateRestrictedFlag = Boolean.FALSE.equals(comp.getIsUpdateRestricted()) ? hasComplaintUpdateRole
				: !hasComplaintUpdateRole;
		comp.setIsUpdateRestricted(isUpdateRestrictedFlag);
		complaintService.deleteBusinessPartnerWhenComplaintClosed(complaint);
	}

	/**
	 * Setting complaint type and status
	 *
	 * @param complaint
	 */
	public void setComplaintValues(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "setComplaintValues");
		ComplaintStatuses status = complaintService.getDefaultStatus();
		ComplaintCategories type = complaintService.getDefaultType();
		if (type != null) {
			complaint.setComplaintTypeCode(type.getCode());
		}
		if (status != null) {
			complaint.setComplaintStatusCode(status.getCode());
		}
		complaint.setLaborUnitCode(Constants.DEFAULT_LABOR_UNIT);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "setComplaintValues");
	}

	public boolean hideStream(Boolean bIsActiveEntity, Boolean bHasDraftEntity, Boolean bHasActiveEntity) {
		Boolean bHideStream = true;
		if (bIsActiveEntity != null && bHasDraftEntity != null && bHasActiveEntity != null) {
			bHideStream = (!bIsActiveEntity && !bHasDraftEntity && !bHasActiveEntity) || bHasActiveEntity;
		}
		return bHideStream;
	}


	/**
	 * calling from Before Update for set old data
	 * values
	 *
	 * @param {@link Complaints} complaint
	 *
	 * @public
	 */
	public void setOldAuditData(Complaints complaint) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER,"setOldAuditData");
		Complaints oldData = complaintService.getComplaintDetails(complaint.getId());
		auditLogHelper.setOldData(oldData);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER,"setOldAuditData");
	}
	/**
	 * calling from After create, Update for audit log data
	 * values
	 *@param {@link Action} action
	 * @param {@link Complaints} newData
	 *
	 * @public
	 */
	public void logUpsert(Action action, Complaints newData) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER,"logUpsert");
		auditLogHelper.logConfigChange(Complaints_.CDS_NAME, action, newData);
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER,"logUpsert");
	}


	@After(event = CdsService.EVENT_READ, entity = ProcessFlow_.CDS_NAME)
	public void afterProcessFlowRead(CdsReadEventContext context, List<ProcessFlow> data) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "afterProcessFlowRead");
		for (ProcessFlow processflow : data) {
			processflow.setSourceIdentifier(processflow.getSourceIdentifier() != null
					? processflow.getSourceIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "")
					: "");
			processflow.setTargetIdentifier(processflow.getTargetIdentifier() != null
					? processflow.getTargetIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "")
					: "");
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "afterProcessFlowRead");
	}

	@After(event = CdsService.EVENT_READ, entity = CommonBusinessObjects_.CDS_NAME)
	public void afterCommonBusinessObjectsRead(CdsReadEventContext context, List<CommonBusinessObjects> data) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_HANDLER, "afterCommonBusinessObjectsRead");
		for (CommonBusinessObjects commonbusinessobjects : data) {
			commonbusinessobjects.setIdentifier(commonbusinessobjects.getIdentifier() != null
					? commonbusinessobjects.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "")
					: "");
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_HANDLER, "afterCommonBusinessObjectsRead");
	}

	@After(event = CdsService.EVENT_READ, entity = QualityNotification_.CDS_NAME)
	public void afterQualityNotificationRead(CdsReadEventContext context,
											 List<QualityNotification> data) {
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
		for (QualityNotification qn : data) {
			qn.setNumber(qn.getIdentifier());
			qn.setIdentifier(
					qn.getIdentifier() != null ? qn.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "") : "");
			qn.setNavigation(
					getNavigationURL(qn.getCompanyId(), readAllDestinations, Constants.QUALITYNOTIFICATION_CODE));
		}
	}

	/**
	 * Hiding button based on the role
	 *
	 * @param data
	 */
	@After(event = CdsService.EVENT_READ, entity = Claim_.CDS_NAME)
	public void afterClaimRead(CdsReadEventContext context, List<Claim> data) {
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
		for (Claim claims : data) {
			claims.setNumber(claims.getIdentifier());
			claims.setIdentifier(
					claims.getIdentifier() != null ? claims.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "") : "");
			claims.setNavigation(
					getNavigationURL(claims.getCompanyId(), readAllDestinations,
							Constants.CLAIM_CODE));
		}
	}

	@After(event = CdsService.EVENT_READ, entity = ReturnPurchaseOrder_.CDS_NAME)
	public void afterReturnPurchaseOrderRead(CdsReadEventContext context,
											 List<ReturnPurchaseOrder> data) {
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
		for (ReturnPurchaseOrder returnPO : data) {
			returnPO.setNumber(returnPO.getIdentifier());
			returnPO.setIdentifier(
					returnPO.getIdentifier() != null ? returnPO.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "")
							: "");
			returnPO.setNavigation(
					getNavigationURL(returnPO.getCompanyId(), readAllDestinations,
							Constants.RETURNPO_CODE));
		}
	}

	@After(event = CdsService.EVENT_READ, entity = Supplier8DProcess_.CDS_NAME)
	public void afterSupplier8DRead(CdsReadEventContext context,
									List<Supplier8DProcess> data) {
		for (Supplier8DProcess eightD : data) {
			eightD.setNumber(eightD.getIdentifier());
			eightD.setIdentifier(
					eightD.getIdentifier() != null ? eightD.getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, "") : "");
			if (eightD.getDefect() != null && eightD.getDefect().getParent() != null
					&& eightD.getDefect().getParent().getIdentifier() != null) {
				eightD.getDefect().getParent().setIdentifier(
						eightD.getDefect().getParent().getIdentifier().replaceFirst(REMOVE_LEADING_ZERO, ""));
			}
		}
	}

	public String getNavigationURL(String companyCode, Iterator<ScpCfDestination> readAllDestinations, String sCode) {
		String sNavigationURL = "";
		Result result = destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(companyCode,
				sCode);
		Object destinationObj = result.first().isPresent() ? result.list().get(0).get("navigationDestination") : null;
		String destination = null != destinationObj ? destinationObj.toString() : "";
		logger.info("Navigation destination is : {} ", destination);
		while (readAllDestinations.hasNext()) {
			ScpCfDestination getDestination = readAllDestinations.next();
			if (null != destination && getDestination.getName().equals(destination) &&
					getDestination.isHttp()) {
				logger.info("Navigation destination name is : {} ",
						getDestination.getName());
				sNavigationURL = getDestination.getName();
			}
		}
		return sNavigationURL;
	}

}

