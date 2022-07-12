package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.*;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.persistence.PersistenceService;

import com.sap.ic.cmh.configuration.handler.TargetReferenceTypeMappingHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.services.cds.CdsService;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.stereotype.Component;
import com.sap.cds.services.messages.Messages;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class ReactivateTargetReferenceTypeMappingsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    TargetReferenceTypeMappingHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(ReactivateTargetReferenceTypeMappingsHandler.class);

    private static final String REACTIVATE_TARGET_REF_TYPE_MAP_HANDLER = "ReactivateTargetReferenceTypeMappings";

    /**
     * On activate event of Target Reference Type Mapping perform update isActive flag
     *
     * @param {@link ReactivateTargetReferenceTypeMappingsContext} context
     *
     * @public
     */
    @On(entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void onReactivateTargetRefTypeMap(final ReactivateTargetReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"onReactivateTargetRefTypeMap");

        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        TargetReferenceTypeMappings targetrefTypeMap = ((CdsService) context.getService()).run(select)
                .single(TargetReferenceTypeMappings.class);
        if (Boolean.TRUE.equals(targetrefTypeMap.getIsActiveEntity())) {
            if(Boolean.TRUE.equals(targetrefTypeMap.getIsActive())){
                sMessageKey = MessageKeys.ACTIVATE_TARGET_REF_TYPE_MAPPING_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(targetrefTypeMap);
                targetrefTypeMap.setIsActive(true);
                CqnUpdate update = Update.entity(TargetReferenceTypeMappings_.class).data(targetrefTypeMap)
                        .where(b -> b.ID().eq(targetrefTypeMap.getId()));
                db.run(update);
                targetrefTypeMap.setIsInActive(false);
                sMessageKey = MessageKeys.ACTIVATION_TARGET_REF_TYPE_MAPPING_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.ACTIVATE_TARGET_REF_TYPE_MAPPING_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(targetrefTypeMap);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, REACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"onReactivateTargetRefTypeMap");

    }
    /**
     * After activate event of Target Reference Type Mapping perform  set audit log
     *
     * @param {@link ReactivateTargetReferenceTypeMappingsContext} context
     *
     * @public
     */
    @After(entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void afterReactivateTargetRefTypeMap(final ReactivateTargetReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"afterReactivateTargetRefTypeMap");
        CqnSelect select = context.getCqn();
        TargetReferenceTypeMappings targetrefTypeMap = ((CdsService) context.getService()).run(select)
                .single(TargetReferenceTypeMappings.class);
        handler.logUpsert(Action.UPDATE, targetrefTypeMap);
        LoggerHelper.logMethodExit(logger, REACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"afterReactivateTargetRefTypeMap");
    }

}