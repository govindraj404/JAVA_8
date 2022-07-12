package com.sap.ic.cmh.masterdata.address.handler;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.address.validations.AddressValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.Addresses_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class AddressHandler implements EventHandler {
	
    /* AddressValidator instance */
    @Autowired
    private AddressValidator addressValidator;

    @Autowired
	private AuditLogHelper<Addresses> auditLogHelper;
    @Autowired
    Messages messages;

    @Autowired
    private AddressService addressService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    public static final Logger logger = LoggerFactory.getLogger(AddressHandler.class);

    private static final String ENTITY_NAME = "Address";

    
    @Before(event = CdsService.EVENT_CREATE, entity = Addresses_.CDS_NAME)
    public void beforeAddressCreate(CdsCreateEventContext context, Addresses addressItem){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeAddressCreate");
        addressValidator.checkInputsSanitized(addressItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeAddressCreate");
    }

    /**
     * This method is used to Create /update Address records
     * @param context cdsCreateContext
     * @param addressItem provided addressItem
     */
    @On(event = CdsService.EVENT_CREATE, entity = Addresses_.CDS_NAME)
    public void onAddressCreate(CdsCreateEventContext context, Addresses addressItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onAddressCreate");
        /* If address details is not found insert else update */
        Addresses addressDetail = addressService.getAddressDetailsBasedOnAddress(addressItem.getAddress());
        if(null!=addressDetail) {
        	String addressId = addressDetail.getId();
        	addressItem.setId(addressId);
        	CqnUpdate update = Update.entity(Addresses_.class).data(addressItem);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onAddressCreate");
    }

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE },entity = Addresses_.CDS_NAME)
    public void afterAddressCreateUpdate(Addresses data) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "afterAddressCreateUpdate");
        logUpsert(Action.CREATE, data);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "afterAddressCreateUpdate");
    }

    @Before(event = { CdsService.EVENT_UPDATE },entity = Addresses_.CDS_NAME)
    public void beforeAddressUpdate(CdsUpdateEventContext context, Addresses data) {
    	LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeAddressUpdate");
    	addressValidator.checkInputsSanitized(data);
        messages.throwIfError();
        setOldAuditData(data);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeAddressUpdate");
    }


    public void setOldAuditData(Addresses addressItem) {
        Addresses oldData;
        try {
            oldData = addressService.getAddress(addressItem.getId());
        } catch (Exception e) {
            oldData = Addresses.create();
        }
        auditLogHelper.setOldData(oldData);
    }

    public void logUpsert(Action action, Addresses newData) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "logUpsert");
        auditLogHelper.logConfigChange(Addresses_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "logUpsert");
    }


}
