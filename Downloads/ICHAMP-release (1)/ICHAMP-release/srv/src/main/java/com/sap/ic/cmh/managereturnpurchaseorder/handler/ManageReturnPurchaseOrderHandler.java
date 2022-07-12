package com.sap.ic.cmh.managereturnpurchaseorder.handler;

import java.util.UUID;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.managereturnpurchaseorder.service.ManageReturnPurchaseOrderService;
import com.sap.ic.cmh.returnpo.handler.ReturnPurchaseOrderHandler;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.managereturnpurchaseorderservice.ManageReturnPurchaseOrderService_;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.managereturnpurchaseorderservice.BusinessObjectStatuses;
import io.micrometer.core.instrument.util.StringUtils;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import org.springframework.util.CollectionUtils;

@Component
@ServiceName(ManageReturnPurchaseOrderService_.CDS_NAME)
public class ManageReturnPurchaseOrderHandler implements EventHandler {

    @Autowired
    ReturnPurchaseOrderHandler returnPurchaseOrderHandler;

    @Autowired
    ComplaintsDao complaintDao;
    @Autowired
    ConfigurationService configurationService;
    @Autowired
    ReturnPurchaseOrderService returnPurchaseOrderService;
    @Autowired
	CommonFunctions commonFunctions;
    @Autowired
    ManageReturnPurchaseOrderService manageReturnPurchaseOrderService;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders> auditLogDifference;

    private static final String ENTITY_NAME = "MANAGE_RETURNPURCHASEORDER";
    private static final String RETURN_PURCHASE_ORDER_HANDLER = "ReturnPurchaseOrderHandler";
    private static final Logger logger = LoggerFactory.getLogger(ManageReturnPurchaseOrderHandler.class);
    private static volatile String returnPurchaseOrderId;

    /**
     * This method is used to call the BEFORE-CREATE event of draft Return Purchase
     * Order Handler validate mandatory attributes to create Return Purchase Order
     * 
     * @param manageReturnPurchaseOrders
     */
    @Before(event = { CdsService.EVENT_CREATE }, entity = ReturnPurchaseOrders_.CDS_NAME)
    public void beforeManageReturnPurchaseOrderCreate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
        LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeManageReturnPurchaseOrderCreate");
        setMasterDataDetails(manageReturnPurchaseOrders);
        cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders = commonFunctions
                .convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders);
        returnPurchaseOrderId = UUID.randomUUID().toString();
        returnPurchaseOrders.setId(returnPurchaseOrderId);
        returnPurchaseOrders.setStatusCode(Constants.STATUS_NEW);
        // Is this scenario possible?
        if (StringUtils.isNotBlank(manageReturnPurchaseOrders.getComplaintCode())) {
            Result comp = complaintDao.getComplaintBasedOnCode(manageReturnPurchaseOrders.getComplaintCode());
            if (comp.first().isPresent()) {
                returnPurchaseOrders.setComplaintId(comp.first().get().get("ID").toString());
                manageReturnPurchaseOrders.setComplaintId(comp.first().get().get("ID").toString());
            }
        } else {
            throw new ServiceException(MessageKeys.COMPLAINT_ASSOCIATION_TO_RETURN_ORDER);
        }
        returnPurchaseOrderHandler.beforeReturnPurchaseOrderCreate(returnPurchaseOrders);
        manageReturnPurchaseOrders.setReturnPurchaseType(returnPurchaseOrders.getReturnPurchaseType());
        manageReturnPurchaseOrders.setItemNumber(returnPurchaseOrders.getItemNumber());
        manageReturnPurchaseOrders.setMaterialId(returnPurchaseOrders.getMaterialId());
        manageReturnPurchaseOrders.setSupplierId(returnPurchaseOrders.getSupplierId());
        manageReturnPurchaseOrders.setPlantId(returnPurchaseOrders.getPlantId());
        manageReturnPurchaseOrders.setPurchasingOrganizationId(returnPurchaseOrders.getPurchasingOrganizationId());
        manageReturnPurchaseOrders.setCompanyId(returnPurchaseOrders.getCompanyId());
        manageReturnPurchaseOrders.setQuantity(returnPurchaseOrders.getQuantity());
        manageReturnPurchaseOrders.setUnit(returnPurchaseOrders.getUnit());
        LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeManageReturnPurchaseOrderCreate");
    }

    /**
     * This method is used to call the ON-CREATE event of draft Return Purchase
     * Order Handler create Return Purchase Order in the configured destination Link
     * the Business object with complaint Insert business object status
     * 
     * @param manageReturnPurchaseOrders
     */
    @On(event = CdsService.EVENT_CREATE, entity = ReturnPurchaseOrders_.CDS_NAME)
    public void onManageReturnPurchaseOrderCreate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
        LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "onManageReturnPurchaseOrderCreate");
        cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders = commonFunctions
                .convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders);
        returnPurchaseOrders.setId(returnPurchaseOrderId);
        returnPurchaseOrderHandler.onReturnPurchaseOrderCreate(returnPurchaseOrders);
        manageReturnPurchaseOrders.setIdentifier(returnPurchaseOrders.getIdentifier());
        manageReturnPurchaseOrders.setStatusCode(returnPurchaseOrders.getStatusCode());
        manageReturnPurchaseOrders.setBusinessObjectStatuses(returnPurchaseOrders.getBusinessObjectStatuses());
        manageReturnPurchaseOrders.setId(returnPurchaseOrders.getId());
        LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "onManageReturnPurchaseOrderCreate");
    }

    /**
     * This method is used to call the AFTER-CREATE event of draft Return Purchase
     * Order Handler update Stream status according to the business object status
     * 
     * @param manageReturnPurchaseOrders
     */
    @After(event = CdsService.EVENT_CREATE, entity = ReturnPurchaseOrders_.CDS_NAME)
    public void afterManageReturnPurchaseOrderCreate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
        LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "afterManageReturnPurchaseOrderCreate");
        cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders = commonFunctions
                .convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders);

        returnPurchaseOrderHandler.afterReturnPurchaseOrderCreate(returnPurchaseOrders);
        logUpsert(manageReturnPurchaseOrders);
        LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "afterManageReturnPurchaseOrderCreate");
    }

    /**
     * This method is used to set the master data details
     * 
     * @param manageClaims
     */
    private void setMasterDataDetails(ReturnPurchaseOrders manageReturnPurchaseOrders) {
        BusinessPartners supplierPerson = configurationService
                .validateSupplierContactPerson(manageReturnPurchaseOrders.getContactPersonCode());
        if (supplierPerson != null) {
            manageReturnPurchaseOrders.setContactPersonId(supplierPerson.getId());
        }
        BusinessPartners personResponsible = configurationService
                .validatePersonResponsibleCode(manageReturnPurchaseOrders.getPersonResponsibleCode());
        if (personResponsible != null) {
            manageReturnPurchaseOrders.setPersonResponsibleId(personResponsible.getId());
        }
    }

    
    /**
     * Check if Return Order
     * @param manageReturnPurchaseOrders
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = ReturnPurchaseOrders_.CDS_NAME)
    public void beforeManageReturnPurchaseOrderUpdate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
    	LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeManageReturnPurchaseOrderUpdate");
        //create a struct and copy the payload
        ReturnPurchaseOrders returnPurchaseOrderstToBeUpdated = Struct.create(ReturnPurchaseOrders.class);
        returnPurchaseOrderstToBeUpdated.putAll(manageReturnPurchaseOrders);
        if(StringUtils.isNotBlank(manageReturnPurchaseOrders.getId())){
        //Get details from DB
    	ReturnPurchaseOrders activeReturnPurchaseOrders = returnPurchaseOrderService.getActiveReturnPurchaseOrders(manageReturnPurchaseOrders.getId());
        //copy the DB details and override the payload
        manageReturnPurchaseOrders.putAll(activeReturnPurchaseOrders);
        //override the RPO payload
        manageReturnPurchaseOrders.putAll(returnPurchaseOrderstToBeUpdated);
    	setOldAuditData(manageReturnPurchaseOrders);
        LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "beforeManageReturnPurchaseOrderUpdate");
    	}
    }
    
    /**
     * Update Return Purchase Order Status from Backend
     * @param manageReturnPurchaseOrders
     */
	@On(event = CdsService.EVENT_UPDATE, entity = ReturnPurchaseOrders_.CDS_NAME)
	public void onManageReturnPurchaseOrderUpdate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
		LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "onManageReturnPurchaseOrderUpdate");
		logger.info("inside update rpo handler");
		logger.info("manageReturnPurchaseOrders ID :: {} ", manageReturnPurchaseOrders.getId());
		cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders = commonFunctions
				.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders);
		List<BusinessObjectStatuses> updateReturnPurchaseOrderStatus = manageReturnPurchaseOrderService
				.updateReturnPurchaseOrderStatus(manageReturnPurchaseOrders);
		manageReturnPurchaseOrders.setBusinessObjectStatuses(
				!CollectionUtils.isEmpty(updateReturnPurchaseOrderStatus) ? updateReturnPurchaseOrderStatus
						: new ArrayList<>());
		returnPurchaseOrders.setStatusCode(manageReturnPurchaseOrders.getStatusCode());
		LoggerHelper.logMethodExit(logger, RETURN_PURCHASE_ORDER_HANDLER, "onManageReturnPurchaseOrderUpdate");
	}
    
    /**
     * Update the Logistics Stream status for updated BO Status
     * @param manageReturnPurchaseOrders
     */
    @After(event = CdsService.EVENT_UPDATE, entity = ReturnPurchaseOrders_.CDS_NAME)
    public void afterManageReturnPurchaseOrderUpdate(ReturnPurchaseOrders manageReturnPurchaseOrders) {
    	LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "afterManageReturnPurchaseOrderUpdate");
    	 cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders=commonFunctions.convertManageReturnPOtoReturnOrders(manageReturnPurchaseOrders);
    	  returnPurchaseOrderHandler.updateStreamStatus(returnPurchaseOrders, Constants.RETURNPO_CODE, true);
          logUpsert(manageReturnPurchaseOrders);
          LoggerHelper.logMethodEntry(logger, RETURN_PURCHASE_ORDER_HANDLER, "afterManageReturnPurchaseOrderUpdate");
    }

    public void setOldAuditData(ReturnPurchaseOrders returnPO) {
		cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders oldData = returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPO.getId());
		auditLogDifference.setOldData(oldData);
	}

	public void logUpsert(ReturnPurchaseOrders returnPO) {
		cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders newData = returnPurchaseOrderService.getReturnPurchaseOrderBasedOnId(returnPO.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

}
