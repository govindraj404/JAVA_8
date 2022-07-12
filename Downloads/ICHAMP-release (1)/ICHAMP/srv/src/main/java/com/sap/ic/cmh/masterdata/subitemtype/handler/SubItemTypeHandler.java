package com.sap.ic.cmh.masterdata.subitemtype.handler;

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
import com.sap.ic.cmh.masterdata.subitemtype.service.SubItemTypeService;
import com.sap.ic.cmh.masterdata.subitemtype.validation.SubItemTypeValidator;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import cds.gen.masterdataservice.SubItemTypes;
import cds.gen.masterdataservice.SubItemTypes_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class SubItemTypeHandler implements EventHandler {

    /* SubItemTypeValidator instance */
    @Autowired
    private SubItemTypeValidator subItemTypeValidator;

    @Autowired
    Messages messages;
    @Autowired
    SubItemTypeService subItemTypeService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    public static final Logger LOGGER = LoggerFactory.getLogger(SubItemTypeHandler.class);

    /**
     *  This method is used to perform Business validation for Sub ItemType
     * @param context
     * @param subItemTypeItem
     */
    @Before(event = CdsService.EVENT_CREATE, entity = SubItemTypes_.CDS_NAME)
    public void beforeSubItemTypeCreate(CdsCreateEventContext context, SubItemTypes subItemTypeItem) {
        LoggerHelper.logMethodEntry(LOGGER, this.getClass().getSimpleName(), "beforeSubItemTypeCreate");
        subItemTypeValidator.validateSubItemTypeFields(subItemTypeItem);   
        messages.throwIfError();
        LoggerHelper.logMethodExit(LOGGER, this.getClass().getSimpleName(), "beforeSubItemTypeCreate");
    }
    
    /**
     *  This method is used to Create/Update Sub Item Type Details
     * @param context
     * @param subItemTypeItem
     */
    @On(event = CdsService.EVENT_CREATE, entity = SubItemTypes_.CDS_NAME)
    public void onSubItemTypeCreate(CdsCreateEventContext context, SubItemTypes subItemTypeItem) {
        LoggerHelper.logMethodEntry(LOGGER, this.getClass().getSimpleName(), "onSubItemTypeCreate");
        SubItemTypes subItemTypeItemDetails = subItemTypeService.fetchSubItemTypes(subItemTypeItem.getCode());
        if(null!=subItemTypeItemDetails){
            CqnUpdate update = Update.entity(SubItemTypes_.class).data(subItemTypeItem);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }

        LoggerHelper.logMethodExit(LOGGER, this.getClass().getSimpleName(), "onSubItemTypeCreate");
    }

    /**
     *  This method is used to perform Business validation for Sub ItemType and Update
     * @param context
     * @param subItemTypeItem
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = SubItemTypes_.CDS_NAME)
    public void beforeSubItemTypeUpdate(CdsUpdateEventContext context, SubItemTypes subItemTypeItem) {
        LoggerHelper.logMethodEntry(LOGGER, this.getClass().getSimpleName(), "beforeSubItemTypeUpdate");
        subItemTypeValidator.validateSubItemTypeFields(subItemTypeItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(LOGGER, this.getClass().getSimpleName(), "beforeSubItemTypeUpdate");
    }
}
