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
import com.sap.ic.cmh.configuration.handler.ComplaintReasonMappingsHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class DeactivateComplaintReasonMappingsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintReasonMappingsHandler handler;
    public static final Logger logger = LoggerHelper.getLogger(DeactivateComplaintReasonMappingsHandler.class);
    private static final String REACTIVATE_COMPLAINT_REASON_MAPPING_HANDLER = "ReactivateComplaintReasonMappingsHandler";
    @On(entity = ComplaintReasonMappings_.CDS_NAME)
    public void deactivateComplaintReasonMappings(final DeactivateComplaintReasonMappingsContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintReasonMappings complaintReasonMappings = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasonMappings.class);
        if (Boolean.TRUE.equals(complaintReasonMappings.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(complaintReasonMappings.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_REASON_MAP_DEACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintReasonMappings);
                complaintReasonMappings.setIsActive(false);
                CqnUpdate update = Update.entity(ComplaintReasonMappings_.class).data(complaintReasonMappings)
                        .where(b -> b.ID().eq(complaintReasonMappings.getId()));
                db.run(update);
                complaintReasonMappings.setIsInActive(true);
                sMessageKey = MessageKeys.COMPLAINT_REASON_MAP_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.COMPLAINT_REASON_MAP_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintReasonMappings);
        context.setCompleted();
    }

    /**
     *
     * After Activate event of ComplaintReasonMapping Object perform validations
     *
     * @public
     */
    @After(entity = ComplaintReasonMappings_.CDS_NAME)
    public void afterDeactivateComplaintReasonMapping(final DeactivateComplaintReasonMappingsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_REASON_MAPPING_HANDLER,"afterDeactivateComplaintReasonMapping");
        CqnSelect select = context.getCqn();
        ComplaintReasonMappings complaintReasonMappings = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasonMappings.class);
        handler.logUpsert(Action.UPDATE, complaintReasonMappings);
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_REASON_MAPPING_HANDLER,"afterDeactivateComplaintReasonMapping");
    }
}
