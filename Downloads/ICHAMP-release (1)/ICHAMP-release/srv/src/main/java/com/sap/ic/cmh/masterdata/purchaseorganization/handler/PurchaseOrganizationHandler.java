package com.sap.ic.cmh.masterdata.purchaseorganization.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.purchaseorganization.service.PurchaseOrganizationService;
import com.sap.ic.cmh.masterdata.purchaseorganization.validation.PurchaseOrganizationValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class PurchaseOrganizationHandler implements EventHandler {

    @Autowired
	CompanyCodeService companyCodeService;

    @Autowired
    private PurchaseOrganizationValidator purchaseOrganizationValidator;

    @Autowired 
    Messages messages;
    @Autowired
    PurchaseOrganizationService purchaseOrganizationService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    public static final Logger logger = LoggerFactory.getLogger(PurchaseOrganizationHandler.class);

    /**
    * Handles input sanity check
    * @param context
    * @param purchaseOrganizationItem
    */
    @Before(event = CdsService.EVENT_CREATE, entity = PurchaseOrganizations_.CDS_NAME)
    public void beforePurchaseOrganizationsCreate(CdsCreateEventContext context, PurchaseOrganizations purchaseOrganizationItem){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforePurchaseOrganizationsCreate");
        purchaseOrganizationValidator.checkInputsSanitized(purchaseOrganizationItem);
        setCompanyCodeId(purchaseOrganizationItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforePurchaseOrganizationsCreate");
    }

    /**
    * Fetching the purchase organization details for the provided purchaseOrganizationItem
    * If purchase organization details is not found insert else update
    * @param context
    * @param purchaseOrganizationItem
    */
    @On(event = CdsService.EVENT_CREATE, entity = PurchaseOrganizations_.CDS_NAME)
    public void onPurchaseOrganizationsCreate(CdsCreateEventContext context, PurchaseOrganizations purchaseOrganizationItem) {
        LoggerHelper.logMethodEntry(logger, "PurchaseOrganizationsHandler", "onPurchaseOrganizationsCreate");
        PurchaseOrganizations purchaseOrgDetails = purchaseOrganizationService.fetchPurchaseOrganization(purchaseOrganizationItem.getPurchaseOrganization());
        if(null!=purchaseOrgDetails) {
        	purchaseOrganizationItem.setId(purchaseOrgDetails.getId());
        	CqnUpdate update = Update.entity(PurchaseOrganizations_.class).data(purchaseOrganizationItem);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onPurchaseOrganizationsCreate");
    }
    
    /**
     * Handles input sanity check
     * @param context
     * @param purchaseOrganizationItem
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = PurchaseOrganizations_.CDS_NAME)	
    public void beforePurchaseOrganizationsUpdate(CdsUpdateEventContext context,  PurchaseOrganizations purchaseOrganizationItem) {	
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforePurchaseOrganizationsUpdate");	
        purchaseOrganizationValidator.checkInputsSanitized(purchaseOrganizationItem);
        setCompanyCodeId(purchaseOrganizationItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforePurchaseOrganizationsUpdate");	
    }

    /**
     * Fetch the company code ID based on code
     * @param purchaseOrganizationItem
     */
	public void setCompanyCodeId(PurchaseOrganizations purchaseOrganizationItem) {
		CompanyCodes companyCode = companyCodeService.fetchCompanyCode(purchaseOrganizationItem.getCompanyCode(),
                MessageKeys.COMPANY_CODE_DOES_NOT_EXIST, PurchaseOrganizations_.class, PurchaseOrganizations_::companyCode);

        if (companyCode != null) {
            purchaseOrganizationItem.setCompanyCodeIDId(companyCode.getId());
        }
	}
}
