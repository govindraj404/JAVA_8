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
import com.sap.ic.cmh.configuration.handler.TargetTypeConfigurationHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cds.services.cds.CdsService;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.stereotype.Component;
import com.sap.cds.services.messages.Messages;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class DeactivateTargetTypesHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;

    @Autowired
    TargetTypeConfigurationHandler targetTypeHandler;

    private static final Logger logger = LoggerFactory.getLogger(DeactivateTargetTypesHandler.class);
    private static final String DEACTIVATE_TARGET_TYPE_HANDLER = "DeactivateTargetTypesHandler";

    /**
     *
     * On Deactivate event of Target Type Object perform validations
     *
     * @public
     */
    @On(entity = TargetTypes_.CDS_NAME)
    public void deactivateTargetTypes(final DeactivateTargetTypesContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        TargetTypes targetTypes = ((CdsService) context.getService()).run(select)
                .single(TargetTypes.class);

        if (Boolean.TRUE.equals(targetTypes.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(targetTypes.getIsActive())){
                sMessageKey = MessageKeys.TARGET_TYPE_DEACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                targetTypeHandler.setOldAuditData(targetTypes);
                targetTypes.setIsActive(false);
                CqnUpdate update = Update.entity(TargetTypes_.class).data(targetTypes)
                        .where(b -> b.ID().eq(targetTypes.getId()));
                db.run(update);
                targetTypes.setIsInActive(true);
                sMessageKey = MessageKeys.TARGET_TYPE_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.TARGET_TYPE_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(targetTypes);
        context.setCompleted();
    }
    /**
     *
     * After Deactivate event of Target Type Object perform validations
     *
     * @public
     */
    @After(entity = TargetTypes_.CDS_NAME)
    public void afterDeactivateTargetTypes(final DeactivateTargetTypesContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_TARGET_TYPE_HANDLER,"afterDeactivateTargetTypes");
        CqnSelect select = context.getCqn();
        TargetTypes targetTypes = ((CdsService) context.getService()).run(select)
                .single(TargetTypes.class);
        targetTypeHandler.logUpsert(Action.UPDATE, targetTypes);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_TARGET_TYPE_HANDLER,"afterDeactivateTargetTypes");
    }
}