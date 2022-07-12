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
import com.sap.ic.cmh.configuration.handler.ComplaintTypeConfigurationHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class DeactivateComplaintTypeConfigurationsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintTypeConfigurationHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateComplaintTypeConfigurationsHandler.class);

    private static final String DEACTIVATE_COMPLAINT_TYPE_HANDLER = "DeactivateComplaintTypeConfigurationsHandler";

    /**
     * on deactivate event of perform deactivation
     *
     * @param {@link DeactivateComplaintTypeConfigurationsContext} context
     *
     * @public
     */
    @On(entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void deactivateComplaintTypeConfigurations(final DeactivateComplaintTypeConfigurationsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_TYPE_HANDLER,"deactivateComplaintTypeConfigurations");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintTypeConfigurations complaintTypeConfigurations = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeConfigurations.class);
        if (Boolean.TRUE.equals(complaintTypeConfigurations.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(complaintTypeConfigurations.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_TYPE_DEACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintTypeConfigurations);
                complaintTypeConfigurations.setIsActive(false);
                CqnUpdate update = Update.entity(ComplaintTypeConfigurations_.class).data(complaintTypeConfigurations)
                        .where(b -> b.ID().eq(complaintTypeConfigurations.getId()));
                db.run(update);
                complaintTypeConfigurations.setIsInActive(true);
                sMessageKey = MessageKeys.COMPLAINT_TYPE_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.COMPLAINT_TYPE_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintTypeConfigurations);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_TYPE_HANDLER,"deactivateComplaintTypeConfigurations");
    }
    /**
     * After deactivate event of perform validations/set Audit Data
     *
     * @param {@link DeactivateComplaintTypeConfigurationsContext} context
     *
     * @public
     */
    @After(entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void afterDeactivateComplaintTypes(final DeactivateComplaintTypeConfigurationsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_TYPE_HANDLER,"afterDeactivateComplaintTypes");
        CqnSelect select = context.getCqn();
        ComplaintTypeConfigurations complaintTypeConfigurations = ((CdsService) context.getService()).run(select)
                .single(ComplaintTypeConfigurations.class);
        handler.logUpsert(Action.UPDATE, complaintTypeConfigurations);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_TYPE_HANDLER,"afterDeactivateComplaintTypes");
    }
}