package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.*;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.handler.ComplaintTypeToItemCatMappingHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class DeactivateComplaintTypeToItemCatMappingHandler implements EventHandler{
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintTypeToItemCatMappingHandler handler;
    public static final Logger logger = LoggerHelper.getLogger(DeactivateComplaintTypeToItemCatMappingHandler.class);

    private static final String DEACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER = "DeactivateComplaintTypeToItemCatMappingHandler";

    /**
     * On Deactivate event of ComplaintTypeToItemCatMaps Object perform validations
     *
     * @public
     */
    @On(entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
    public void onDeactivateComplaintTypeToItemCatMaps(final DeactivateComplaintTypeToItemCategoryMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "onDeactivateComplaintTypeToItemCatMaps");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeToItemCategoryMappings.class);
        if (Boolean.TRUE.equals(complaintTypeToItemCategoryMappings.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(complaintTypeToItemCategoryMappings.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_DEACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintTypeToItemCategoryMappings);
                complaintTypeToItemCategoryMappings.setIsActive(false);
                CqnUpdate update = Update.entity(ComplaintTypeToItemCategoryMappings_.class).data(complaintTypeToItemCategoryMappings)
                        .where(b -> b.ID().eq(complaintTypeToItemCategoryMappings.getId()));
                db.run(update);
                complaintTypeToItemCategoryMappings.setIsInActive(true);
                sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintTypeToItemCategoryMappings);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "onDeactivateComplaintTypeToItemCatMaps");

    }

    /**
     * After Deactivate event of ComplaintTypeToItemCatMaps Object perform validations
     *
     * @public
     */
    @After(entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
    public void afterDeactivateComplaintTypeToItemCatMaps(final DeactivateComplaintTypeToItemCategoryMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "afterDeactivateComplaintTypeToItemCatMaps");
        CqnSelect select = context.getCqn();
        ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeToItemCategoryMappings.class);
        handler.logUpsert(Action.UPDATE, complaintTypeToItemCategoryMappings);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "afterDeactivateComplaintTypeToItemCatMaps");
    }
}
