package com.sap.ic.cmh.masterdata.companycode.handler;

import org.slf4j.Logger;
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
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.companycode.validations.CompanyCodeValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class CompanyCodeHandler implements EventHandler {

    @Autowired
    private AddressService addressService;

    /* CompanyCodeValidator instance */
    @Autowired
    private CompanyCodeValidator companyCodeValidator;
    @Autowired
    CompanyCodeService companyCodeService;
    
    @Autowired
    Messages messages;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    public static final Logger logger = LoggerHelper.getLogger(CompanyCodeHandler.class);

    
    /**
     * Method used to perform Business Validation
     * @param context
     * @param companyCodeItem
     */
    @Before(event = CdsService.EVENT_CREATE, entity = CompanyCodes_.CDS_NAME)
    public void beforeCompanyCodeCreate(CdsCreateEventContext context, CompanyCodes companyCodeItem){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeCompanyCodeCreate");
        /* Input sanitization process */
        companyCodeValidator.checkInputsSanitized(companyCodeItem);
        Addresses address = addressService.fetchAddress(companyCodeItem.getAddress(),
                MessageKeys.ADDRESS_DOES_NOT_EXIST, CompanyCodes_.class, CompanyCodes_::address);
        if (address != null) {
            companyCodeItem.setAddressIDId(address.getId());
        }
        messages.throwIfError();

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeCompanyCodeCreate");
    }
    
    /**
     * Method used to create/update company code
     * @param context
     * @param companyCodeItem
     */
    @On(event = CdsService.EVENT_CREATE, entity = CompanyCodes_.CDS_NAME)
    public void onCompanyCodeCreate(CdsCreateEventContext context, CompanyCodes companyCodeItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onCompanyCodeCreate");

        /* Checking if provided companyCode Item is present in database */
        CompanyCodes companyCodeDetails = companyCodeService.fetchCompanyCodesBasedOnCode(companyCodeItem.getCompanyCode());
        if(null!=companyCodeDetails) {
        	String companyCodeId = companyCodeDetails.getId();
        	companyCodeItem.setId(companyCodeId);
        	CqnUpdate update = Update.entity(CompanyCodes_.class).data(companyCodeItem);
            context.setResult(cdsService.run(update));
            context.setCompleted();
        }      
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onCompanyCodeCreate");
    }
    
    /**
     * Method used to perform Business Validation
     * @param context
     * @param companyCodeItem
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = CompanyCodes_.CDS_NAME)	
    public void beforeCompanyCodeUpdate(CdsUpdateEventContext context,  CompanyCodes companyCodeItem) {	
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeCompanyCodeUpdate");	
        companyCodeValidator.checkInputsSanitized(companyCodeItem);
        messages.throwIfError();	
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeCompanyCodeUpdate");	
    }
}
