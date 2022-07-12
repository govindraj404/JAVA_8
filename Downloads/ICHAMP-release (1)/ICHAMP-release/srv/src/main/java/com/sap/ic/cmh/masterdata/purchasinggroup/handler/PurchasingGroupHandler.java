package com.sap.ic.cmh.masterdata.purchasinggroup.handler;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.purchasinggroup.service.PurchasingGroupService;
import com.sap.ic.cmh.masterdata.purchasinggroup.validation.PurchasingGroupValidator;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import cds.gen.masterdataservice.PurchasingGroups;
import cds.gen.masterdataservice.PurchasingGroups_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class PurchasingGroupHandler implements EventHandler {

    @Autowired
    Messages messages;

    @Autowired
    private PurchasingGroupValidator purchasingGroupValidator;
    @Autowired
    PurchasingGroupService purchasingGroupService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;
      
    public static final Logger logger = LoggerFactory.getLogger(PurchasingGroupHandler.class);

    /**
     * This method is to perform Business Validation for create
     * @param context
     * @param purchasingGroup
     */
    @Before(event = CdsService.EVENT_CREATE, entity = PurchasingGroups_.CDS_NAME)
    public void beforePurchasingGroupOnCreate(CdsCreateEventContext context, PurchasingGroups purchasingGroup) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforePurchasingGroupOnCreate");
        purchasingGroupValidator.checkInputsSanitized(purchasingGroup);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforePurchasingGroupOnCreate");
    } 
    
    /**
     * Fetching the purchasing group details for the provided purchasingGroup
    * If purchasing group details is not found insert else update
     * @param context
     * @param purchasingGroup
     */
    @On(event = CdsService.EVENT_CREATE, entity = PurchasingGroups_.CDS_NAME)
    public void onPurchasingGroupOnCreate(CdsCreateEventContext context, PurchasingGroups purchasingGroup){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onPurchasingGroupOnCreate");
        PurchasingGroups purchasingGroupsDetail = purchasingGroupService.fetchPurchasingGroupDetails(purchasingGroup.getCode());
        if(null!=purchasingGroupsDetail) {
        	CqnUpdate update = Update.entity(PurchasingGroups_.class).data(purchasingGroup);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onPurchasingGroupOnCreate");
    }
    
    /**
     * This method is to perform Business Validation for update
     * @param context
     * @param purchasingGroup
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = PurchasingGroups_.CDS_NAME)
    public void validatePurchasingGroupOnUpdate(CdsUpdateEventContext context, PurchasingGroups purchasingGroup) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validatePurchasingGroupOnUpdate");
        purchasingGroupValidator.checkInputsSanitized(purchasingGroup);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validatePurchasingGroupOnUpdate");
    }
}
