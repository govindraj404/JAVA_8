package com.sap.ic.cmh.configuration.action.handler;


import com.sap.cds.services.auditlog.Action;
import cds.gen.configurationservice.*;
import com.sap.cds.services.handler.annotations.After;
import com.sap.ic.cmh.configuration.handler.TargetTypeConfigurationHandler;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.gen.MessageKeys;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cds.services.cds.CdsService;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.stereotype.Component;
import com.sap.cds.services.messages.Messages;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class ReactivateTargetTypesHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;

    @Autowired
    TargetTypeConfigurationHandler targetTypeHandler;


    private static final Logger logger = LoggerHelper.getLogger(ReactivateTargetTypesHandler.class);
    private static final String REACTIVATE_TARGET_TYPE_HANDLER = "ReactivateTargetTypesHandler";

    /**
     *
     * On Reactivate event of Target Type Object perform validations
     *
     * @public
     */
    @On(entity = TargetTypes_.CDS_NAME)
    public void reactivateTargetTypes(final ReactivateTargetTypesContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();

        TargetTypes targetTypes = ((CdsService) context.getService()).run(select)
                .single(TargetTypes.class);

        if (Boolean.TRUE.equals(targetTypes.getIsActiveEntity())) {
            if(Boolean.TRUE.equals(targetTypes.getIsActive())){
                sMessageKey = MessageKeys.TARGET_TYPE_ACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                targetTypeHandler.setOldAuditData(targetTypes);
                targetTypes.setIsActive(true);
                CqnUpdate update = Update.entity(TargetTypes_.class).data(targetTypes)
                        .where(b -> b.ID().eq(targetTypes.getId()));
                db.run(update);
                targetTypes.setIsInActive(false);
                sMessageKey = MessageKeys.TARGET_TYPE_ACTIVATION_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.TARGET_TYPE_ACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(targetTypes);
        context.setCompleted();
    }
    /**
     *
     * After Reactivate event of Target Type Object perform validations
     *
     * @public
     */
    @After(entity = TargetTypes_.CDS_NAME)
    public void afterReactivateTargetTypes(final ReactivateTargetTypesContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_TARGET_TYPE_HANDLER,"afterReactivateTargetTypes");
        CqnSelect select = context.getCqn();
        TargetTypes targetTypes = ((CdsService) context.getService()).run(select)
                .single(TargetTypes.class);
        targetTypeHandler.logUpsert(Action.UPDATE, targetTypes);
        LoggerHelper.logMethodExit(logger, REACTIVATE_TARGET_TYPE_HANDLER,"afterReactivateTargetTypes");
    }


}