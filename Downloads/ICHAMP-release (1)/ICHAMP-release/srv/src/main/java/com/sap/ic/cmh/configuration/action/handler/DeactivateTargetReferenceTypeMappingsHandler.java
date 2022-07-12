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
public class DeactivateTargetReferenceTypeMappingsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    TargetReferenceTypeMappingHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateTargetReferenceTypeMappingsHandler.class);

    private static final String DEACTIVATE_TARGET_REF_TYPE_MAP_HANDLER = "DeactivateTargetReferenceTypeMappingsHandler";

    /**
     * On Deactivate event of Target Reference Type Mapping perform update isActive flag
     *
     * @param {@link DeactivateTargetReferenceTypeMappingsContext} context
     *
     * @public
     */
    @On(entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void onDeactivateTargetRefTypeMappings(final DeactivateTargetReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"onDeactivateTargetRefTypeMappings");

        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        TargetReferenceTypeMappings targetRefTypeMap = ((CdsService) context.getService()).run(select)
                .single(TargetReferenceTypeMappings.class);
        if (Boolean.TRUE.equals(targetRefTypeMap.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(targetRefTypeMap.getIsActive())){
                sMessageKey = MessageKeys.DEACTIVATE_TARGET_REF_TYPE_MAPPING_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(targetRefTypeMap);
                targetRefTypeMap.setIsActive(false);
                CqnUpdate update = Update.entity(TargetReferenceTypeMappings_.class).data(targetRefTypeMap)
                        .where(b -> b.ID().eq(targetRefTypeMap.getId()));
                db.run(update);
                targetRefTypeMap.setIsInActive(true);
                sMessageKey = MessageKeys.DEACTIVATION_TARGET_REF_TYPE_MAPPING_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.DEACTIVATE_TARGET_REF_TYPE_MAPPING_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(targetRefTypeMap);
        context.setCompleted();

        LoggerHelper.logMethodExit(logger, DEACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"onDeactivateTargetRefTypeMappings");

    }
    /**
     * After Deactivate event of Target Reference Type Mapping perform set audit log data
     *
     * @param {@link DeactivateTargetReferenceTypeMappingsContext} context
     *
     * @public
     */
    @After(entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void afterDeactivateTargetRefTypeMap(final DeactivateTargetReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"afterDeactivateTargetRefTypeMap");
        CqnSelect select = context.getCqn();
        TargetReferenceTypeMappings targetrefTypeMap = ((CdsService) context.getService()).run(select)
                .single(TargetReferenceTypeMappings.class);
        handler.logUpsert(Action.UPDATE, targetrefTypeMap);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_TARGET_REF_TYPE_MAP_HANDLER,"afterDeactivateTargetRefTypeMap");
    }
}