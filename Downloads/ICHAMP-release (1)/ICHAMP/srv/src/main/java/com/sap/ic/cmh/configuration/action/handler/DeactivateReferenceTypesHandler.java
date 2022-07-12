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
import com.sap.ic.cmh.configuration.handler.ReferenceTypeHandler;
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
public class DeactivateReferenceTypesHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ReferenceTypeHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateReferenceTypesHandler.class);
    private static final String DEACTIVATE_REFERENCE_TYPE_HANDLER = "DeactivateReferenceTypesHandler";

    /**
     *
     * On Deactivate event of ReferenceType Object perform validations
     *
     * @public
     */
    @On(entity = ReferenceTypes_.CDS_NAME)
    public void deactivateReferenceTypes(final DeactivateReferenceTypesContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ReferenceTypes referenceTypes = ((CdsService) context.getService()).run(select)
                .single(ReferenceTypes.class);
        if (Boolean.TRUE.equals(referenceTypes.getIsActiveEntity())) {
            if (Boolean.FALSE.equals(referenceTypes.getIsActive())) {
                sMessageKey = MessageKeys.REFERENCE_TYPE_DEACTIVATE_NOT_SUCCESSFUL;
            } else {
                handler.setOldAuditData(referenceTypes);
                referenceTypes.setIsActive(false);
                CqnUpdate update = Update.entity(ReferenceTypes_.class).data(referenceTypes)
                        .where(b -> b.ID().eq(referenceTypes.getId()));
                db.run(update);
                referenceTypes.setIsInActive(true);
                sMessageKey = MessageKeys.REFERENCE_TYPE_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.REFERENCE_TYPE_DRAFT_DEACTIVATION_FAILED;
        }

        messages.success(sMessageKey);
        context.setResult(referenceTypes);
        context.setCompleted();
    }

    /**
     *
     * After Deactivate event of ReferenceType Object perform validations
     *
     * @public
     */
    @After(entity = ReferenceTypes_.CDS_NAME)
    public void afterDeactivateReferenceType(final DeactivateReferenceTypesContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_REFERENCE_TYPE_HANDLER,"afterDeactivateReferenceType");
        CqnSelect select = context.getCqn();
        ReferenceTypes referenceTypes = ((CdsService) context.getService()).run(select)
                .single(ReferenceTypes.class);
        handler.logUpsert(Action.UPDATE, referenceTypes);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_REFERENCE_TYPE_HANDLER,"afterDeactivateReferenceType");
    }
}
