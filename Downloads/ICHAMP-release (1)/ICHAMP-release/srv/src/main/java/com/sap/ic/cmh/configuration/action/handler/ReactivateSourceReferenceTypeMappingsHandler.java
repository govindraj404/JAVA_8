package com.sap.ic.cmh.configuration.action.handler;

import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.auditlog.Action;
import cds.gen.configurationservice.*;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.handler.SourceReferenceTypeMappingHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class ReactivateSourceReferenceTypeMappingsHandler implements EventHandler {

    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;

    @Autowired
    SourceReferenceTypeMappingHandler sourceReferenceTypeMappingsHandler;

    private static final Logger logger = LoggerHelper.getLogger(ReactivateSourceReferenceTypeMappingsHandler.class);
    private static final String REACTIVATE_SOURCE_REFERENCE_TYPE_MAPPING_HANDLER = "ReactivateSourceReferenceTypeMappingsHandler";

    /**
     *
     * On Reactivate event of SourceReferenceTypeMappings Object perform validations
     *
     * @public
     */
    @On(entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void reactivateSourceReferenceTypeMappings(final ReactivateSourceReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"reactivateSourceReferenceTypeMappings");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        SourceReferenceTypeMappings sourceReferenceTypeMappings = ((CdsService) context.getService()).run(select).single(SourceReferenceTypeMappings.class);
        if (Boolean.TRUE.equals(sourceReferenceTypeMappings.getIsActiveEntity())) {
            if (Boolean.TRUE.equals(sourceReferenceTypeMappings.getIsActive())) {
                sMessageKey = MessageKeys.SOURCE_REFRENCE_TYPE_ACTIVATE_NOT_SUCCESSFUL;
            } else {
                sourceReferenceTypeMappingsHandler.setOldAuditData(sourceReferenceTypeMappings);
                sourceReferenceTypeMappings.setIsActive(true);
                CqnUpdate update = Update.entity(SourceReferenceTypeMappings_.class).data(sourceReferenceTypeMappings).where(b -> b.ID().eq(sourceReferenceTypeMappings.getId()));
                db.run(update);
                sourceReferenceTypeMappings.setIsInActive(false);
                sMessageKey = MessageKeys.SOURCE_REFRENCE_TYPE_ACTIVATION_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.SOURCE_REFRENCE_TYPE_ACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(sourceReferenceTypeMappings);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, REACTIVATE_SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"reactivateSourceReferenceTypeMappings");
    }
    /**
     *
     * After Reactivate event of SourceReferenceTypeMappings Object perform validations
     *
     * @public
     */
    @After(entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void afterReactivateSourceReferenceTypeMappings(final ReactivateSourceReferenceTypeMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterReactivateSourceReferenceTypeMappings");
        CqnSelect select = context.getCqn();
        SourceReferenceTypeMappings sourceReferenceTypeMappings = ((CdsService) context.getService()).run(select)
                .single(SourceReferenceTypeMappings.class);
        sourceReferenceTypeMappingsHandler.logUpsert(Action.UPDATE, sourceReferenceTypeMappings);
        LoggerHelper.logMethodExit(logger, REACTIVATE_SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterReactivateSourceReferenceTypeMappings");
    }

}