package com.sap.ic.cmh.returnpo.handler;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
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
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.returnpo.validation.ReturnPurchaseOrderValidation;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.apache.commons.lang3.StringUtils;
import cds.gen.complaintservice.Complaints;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders_;
import cds.gen.returnpurchaseorderservice.Addresses_;
import cds.gen.returnpurchaseorderservice.BusinessPartners_;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;

@Component
@ServiceName("ReturnPurchaseOrderService")
public class ReturnPurchaseOrderHandler implements EventHandler {

	@Autowired
	ReturnPurchaseOrderService returnPurchaseOrderService;
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
	ReturnPurchaseOrderValidation returnPurchaseOrderValidation;

	@Autowired
	DestinationService destinationService;

	@Autowired
	DestinationConfigurationDao destinationConfigurationDao;
	@Autowired
	CommonFunctions commonFunctions;

	@Autowired
	private AuditLogDifference<ReturnPurchaseOrders> auditLogDifference;

	private static final String RETURN_PURCHASE_ORDER_HANDLER = "ReturnPurchaseOrderHandler";
	private static final Logger logger = LoggerFactory.getLogger(ReturnPurchaseOrderHandler.class);

	private static final String BO_TYPE = Constants.RETURNPO_CODE;
	private static final boolean IS_ACTIVE = false;
	private static final String ENTITY_NAME = "ReturnPurchaseOrderHandler";

	@Before(event = DraftService.EVENT_DRAFT_NEW)
	public void beforeReturnPurchaseOrderDraft(DraftNewEventContext context, ReturnPurchaseOrders returnPO) {
		returnPO.setStatusCode(Constants.STATUS_NEW);
		if (returnPO.getComplaintId() != null) {
			ReturnPurchaseOrders oldDraftRPO = returnPurchaseOrderService
					.getDraftReturnOrderByComplaintID(returnPO.getComplaintId());
			if (oldDraftRPO != null) {
				returnPO.putAll(oldDraftRPO);
				returnPurchaseOrderService.deleteDraftReturnOrderByID(oldDraftRPO.getId());
			}
		}
		beforeReturnPurchaseOrderPatch(returnPO);
	}

	// @Before(event = DraftService.EVENT_DRAFT_PATCH)
	public void beforeReturnPurchaseOrderPatch(ReturnPurchaseOrders returnPO) {
		if (returnPO.getComplaintId() != null) {
			// set the master data details to the QN from the complaint
			Complaints complaintDetails = setMasterDataFromComplaints(returnPO);
			String complaintTypeCode = null != complaintDetails ? complaintDetails.getComplaintTypeCode() : "";
			returnPurchaseOrderService.setConfiguredValues(returnPO, BO_TYPE, complaintTypeCode);
		}
	}

	@Before(event = { CdsService.EVENT_CREATE }, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void beforeReturnPurchaseOrderCreate(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeReturnPurchaseOrderCreate");
		beforeReturnPurchaseOrderPatch(returnPO);
		complaintService.validateComplaintStatus(returnPO.getComplaintId());
		returnPurchaseOrderService.validateIfReturnPurchaseOrderExistsForComplaint(returnPO.getComplaintId());
		returnPurchaseOrderService.validateReturnPurchaseOrderFields(returnPO);
		messages.throwIfError();
	}

	@On(event = CdsService.EVENT_CREATE, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void onReturnPurchaseOrderCreate(ReturnPurchaseOrders returnPO) {
		logger.info("Inside On create event of Return Purchase Order");
		returnPurchaseOrderService.createReturPurchaseOrder(returnPO);
		String backendStatusCode = Constants.RPO_BACKEND_STATUS_CREATED;
		businessObjectService.updateBusinessObjects(returnPO, BO_TYPE);
		setBusinessObjectStatus(returnPO, backendStatusCode, IS_ACTIVE);

		complaintService.updateComplaintStatus(returnPO.getComplaintId(), Constants.COMPLAINT_IN_PROGRESS);

	}

	public void setBusinessObjectStatus(ReturnPurchaseOrders returnPO, String backendStatusCode, boolean isActive) {
		/*
		 * Get the corresponding CMH cockpit status of QN for the backend status and
		 * insert in BusinessObjectStatus entity
		 */
		businessObjectService.setBusinessObjectStatus(BO_TYPE, backendStatusCode, returnPO.getId(), isActive);
		/*
		 * get the current BO status and set it to the status attribute to display the
		 * latest BO status in UI
		 */
		String currentStatus = businessObjectService.getCurrentBOStatus(returnPO.getId());
		returnPO.setStatusCode(currentStatus);
	}

	@After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void afterReturnPurchaseOrderCreate(ReturnPurchaseOrders returnPO) {
		updateStreamStatus(returnPO, BO_TYPE, IS_ACTIVE);
		logger.info("Creation of RPO is completed successfully.");
		logUpsert(returnPO);
	}

	@Before(event = { CdsService.EVENT_UPDATE }, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void beforeReturnPurchaseOrderUpdate(ReturnPurchaseOrders returnPO) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeReturnPurchaseOrderUpdate");
		returnPurchaseOrderService.validateIfReturnPurchaseOrderExists(returnPO);
		messages.throwIfError();
		setOldAuditData(returnPO);
	}

	public void updateStreamStatus(ReturnPurchaseOrders returnPO, String boType, boolean isActive) {
		streamService.updateStreamStatus(returnPO.getId(), boType, isActive);
	}

	@Before(event = CdsService.EVENT_READ, entity = Addresses_.CDS_NAME)
	public void beforeReturnPurchaseOrderReadAddress(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeReturnPurchaseOrderReadAddress");
		logger.info("Before Read of Address in Return Purchase Order");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeReturnPurchaseOrderReadAddress");
	}

	@Before(event = CdsService.EVENT_READ, entity = BusinessPartners_.CDS_NAME)
	public void beforeReturnPurchaseOrderReadBusinessPartner(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER,
				"beforeReturnPurchaseOrderReadBusinessPartner");
		logger.info("Before Read of Business Partners in Return Purchase Order");
		commonFunctions.checkBeforeRead(context);
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER,
				"beforeReturnPurchaseOrderReadBusinessPartner");
	}

	@After(event = CdsService.EVENT_READ, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void afterReturnPurchaseOrderReadUpdate(CdsReadEventContext context, List<ReturnPurchaseOrders> data) {
		UserInfo user = context.getUserInfo();
		String companyCode = "";
		boolean hasReturnPurchaseOrderUpdateRole = user.hasRole("ReturnPurchaseOrder.Update");
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Iterator<ScpCfDestination> readAllDestinations = destinationService.readAllDestination(scpCfDestinationLoader);
		for (ReturnPurchaseOrders returnPO : data) {
			returnPO.setNumber(returnPO.getIdentifier());
			returnPO.setIdentifier(
					returnPO.getIdentifier() != null ? returnPO.getIdentifier().replaceFirst("^0+(?!$)", "") : "");
			setFieldControl(returnPO);
			returnPO.setIsUpdateRestricted(!hasReturnPurchaseOrderUpdateRole);
			companyCode = returnPO.getCompanyId();
			ReturnPurchaseOrders returnPODetails = returnPurchaseOrderService
					.getReturnOrderStatusAndCompanyCode(returnPO.getId());
			if (returnPODetails != null && StringUtils.isNotBlank(returnPODetails.getStatusCode())) {
				if (!returnPODetails.getStatusCode().equalsIgnoreCase(Constants.STATUS_NEW)) {
					returnPO.setIsReturnOrderFieldControl(Constants.FIELD_CONTROL_READ_ONLY);
					returnPO.setIsReturnOrderFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
				}
				if (returnPODetails.getStatusCode().equalsIgnoreCase(Constants.RPO_STATUS_CLOSED)
						&& Boolean.FALSE.equals(returnPO.getIsUpdateRestricted())) {
					returnPO.setIsUpdateRestricted(hasReturnPurchaseOrderUpdateRole);
				}
				companyCode = returnPODetails.getCompanyId();
			}
			/* START of Set Navigation URL */
			setNavigationURL(companyCode, readAllDestinations, returnPO);
			/* END of Set Navigation URL */
		}
	}

	public void setFieldControl(ReturnPurchaseOrders returnPO) {
		if (null != returnPO.getIsActiveEntity() && null != returnPO.getHasActiveEntity()) {
			logger.info("IsActiveEntity True! : ");
			if (Boolean.TRUE.equals(returnPO.getIsActiveEntity())
					&& Boolean.FALSE.equals(returnPO.getHasActiveEntity())) {
				returnPO.setIsReturnOrderFieldControlMandatory(Constants.FIELD_CONTROL_READ_ONLY);
			} else {
				logger.info("Else condition : ");
				returnPO.setIsReturnOrderFieldControlMandatory(Constants.FIELD_CONTROL_MANDATORY);
			}
		}
	}

	/**
	 * Set Navigation URL
	 * 
	 * @param companyCode
	 * @param readAllDestinations
	 * @param returnPO
	 */
	public void setNavigationURL(String companyCode, Iterator<ScpCfDestination> readAllDestinations,
			ReturnPurchaseOrders returnPO) {
		Result result = destinationConfigurationDao.getDestinationConfigBasedOnCompanyAndBOType(companyCode,
				Constants.RETURNPO_CODE);
		Object destinationObj = result.first().isPresent() ? result.list().get(0).get("navigationDestination") : null;
		String destination = null != destinationObj ? destinationObj.toString() : "";
		logger.info("Navigation Destination is : {} ", destination);
		while (readAllDestinations.hasNext()) {
			ScpCfDestination getDestination = readAllDestinations.next();
			logger.info("Navigation Destination name is :{} ", getDestination.getName());
			if (null != destination && getDestination.getName().equals(destination) && getDestination.isHttp()) {
				logger.info("Navigating HTTP Destination is : {} ", getDestination.getName());
				returnPO.setSNavigation(getDestination.getName());
			}
		}
	}

	private Complaints setMasterDataFromComplaints(ReturnPurchaseOrders returnPO) {
		String complaintID = returnPO.getComplaintId();
		Complaints complaint = complaintService.getMasterDataFromComplaints(complaintID);
		if (null != complaint) {
			returnPO.setMaterialId(complaint.getMaterialId());
			returnPO.setPlantId(complaint.getPlantId());
			returnPO.setSupplierId(complaint.getSupplierId());
			returnPO.setPurchasingOrganizationId(complaint.getPurchasingOrganizationId());
			returnPO.setQuantity(complaint.getQuantity());
			returnPO.setUnit(complaint.getUnitCode());
			returnPO.setCompanyId(complaint.getCompanyCodeId());
		}

		return complaint;

	}

	public void setOldAuditData(ReturnPurchaseOrders returnPO) {
		ReturnPurchaseOrders oldData = returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPO.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void logUpsert(ReturnPurchaseOrders returnPO) {
		ReturnPurchaseOrders newData = returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPO.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

}