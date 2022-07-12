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
public class ReactivateComplaintTypeToItemCatMappingHandler implements EventHandler{
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintTypeToItemCatMappingHandler handler;
    public static final Logger logger = LoggerHelper.getLogger(ReactivateComplaintTypeToItemCatMappingHandler.class);

    private static final String REACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER = "ReactivateComplaintTypeToItemCatMappingHandler";

    /**
     * On Reactivate event of ComplaintTypeToItemCatMaps Object perform validations
     *
     * @public
     */
    @On(entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
    public void onReactivateComplaintTypeToItemCatMaps(final ReactivateComplaintTypeToItemCategoryMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "onReactivateComplaintTypeToItemCatMaps");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintTypeToItemCategoryMappings complaintTypeToItemCatMaps = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeToItemCategoryMappings.class);
        if (Boolean.TRUE.equals(complaintTypeToItemCatMaps.getIsActiveEntity())) {
            if(Boolean.TRUE.equals(complaintTypeToItemCatMaps.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_ACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintTypeToItemCatMaps);
                complaintTypeToItemCatMaps.setIsActive(true);
                CqnUpdate update = Update.entity(ComplaintTypeToItemCategoryMappings_.class).data(complaintTypeToItemCatMaps)
                        .where(b -> b.ID().eq(complaintTypeToItemCatMaps.getId()));
                db.run(update);
                complaintTypeToItemCatMaps.setIsInActive(false);
                sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_ACTIVATION_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.COMPLAINT_ITEMCAT_MAP_ACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintTypeToItemCatMaps);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "onReactivateComplaintTypeToItemCatMaps");
    }
    /**
     * After Reactivate event of ComplaintTypeToItemCatMaps Object perform validations
     *
     * @public
     */
    @After(entity = ComplaintTypeToItemCategoryMappings_.CDS_NAME)
    public void afterReactivateComplaintTypeToItemCatMaps(final ReactivateComplaintTypeToItemCategoryMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "afterReactivateComplaintTypeToItemCatMaps");
        CqnSelect select = context.getCqn();
        ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeToItemCategoryMappings.class);
        handler.logUpsert(Action.UPDATE, complaintTypeToItemCategoryMappings);
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_TYPE_TO_ITEM_CAT_MAPPING_HANDLER, "afterReactivateComplaintTypeToItemCatMaps");
    }
}
