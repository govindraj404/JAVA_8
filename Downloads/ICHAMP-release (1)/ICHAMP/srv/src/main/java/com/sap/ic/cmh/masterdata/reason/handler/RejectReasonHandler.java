package com.sap.ic.cmh.masterdata.reason.handler;

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
import com.sap.ic.cmh.masterdata.reason.service.RejectReasonService;
import com.sap.ic.cmh.masterdata.reason.validation.RejectReasonValidator;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import cds.gen.masterdataservice.Reasons;
import cds.gen.masterdataservice.Reasons_;


@Component
@RequestScope
@ServiceName("MasterDataService")
public class RejectReasonHandler implements EventHandler {

    @Autowired
    private RejectReasonValidator rejectReasonValidator;
    @Autowired
    Messages messages;
    @Autowired
    RejectReasonService rejectReasonService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;
    
    public static final Logger logger = LoggerFactory.getLogger(RejectReasonHandler.class);
   
    
    /**
     * This method is to perform Business Validation for create
     * @param context
     * @param reasons
     */
    @Before(event = CdsService.EVENT_CREATE, entity = Reasons_.CDS_NAME)
    public void validateRejectReasonOnCreate(CdsCreateEventContext context, Reasons reasons) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateRejectReasonOnCreate");
        rejectReasonValidator.checkInputsSanitized(reasons);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateRejectReasonOnCreate");
    }
    
    /**
     * Fetching the Reject Reason details for the provided reasons
    * If Reject Reason details is not found insert else update
     * @param context
     * @param reasons
     */
    @On(event = CdsService.EVENT_CREATE, entity = Reasons_.CDS_NAME)
    public void onRejectReasonOnCreate(CdsCreateEventContext context, Reasons reasons) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onRejectReasonOnCreate");
        Reasons reasonDetails = rejectReasonService.fetchReasonBasedOnCode(reasons.getCode());
        if(null!=reasonDetails){
            CqnUpdate update = Update.entity(Reasons_.class).data(reasons);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onRejectReasonOnCreate");
    }
    

    /**
     * This method is to perform Business Validation for update
     * @param context
     * @param reasons
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = Reasons_.CDS_NAME)
    public void validateRejectReasonOnUpdate(CdsUpdateEventContext context, Reasons reasons) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateRejectReasonOnUpdate");
        rejectReasonValidator.checkInputsSanitized(reasons);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateRejectReasonOnUpdate");
    }  
    
}