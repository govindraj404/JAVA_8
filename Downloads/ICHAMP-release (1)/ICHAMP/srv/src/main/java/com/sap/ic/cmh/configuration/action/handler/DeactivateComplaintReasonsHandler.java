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
import com.sap.ic.cmh.configuration.handler.ComplaintReasonHandler;
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
public class DeactivateComplaintReasonsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintReasonHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateComplaintReasonsHandler.class);

    private static final String DEACTIVATE_COMPLAINT_REASON_HANDLER = "DeactivateComplaintReasonsHandler";

    /**
     * on deactivate event of perform deactivation
     *
     * @param {@link DeactivateComplaintReasonsContext} context
     *
     * @public
     */
    @On(entity = ComplaintReasons_.CDS_NAME)
    public void onDeactivateComplaintReasons(final DeactivateComplaintReasonsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_REASON_HANDLER,"onDeactivateComplaintReasons");

        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintReasons complaintReasons = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasons.class);
        if (Boolean.TRUE.equals(complaintReasons.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(complaintReasons.getIsActive())){
                sMessageKey = MessageKeys.DEACTIVATE_COMPLAINT_REASON_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintReasons);

                complaintReasons.setIsActive(false);
                CqnUpdate update = Update.entity(ComplaintReasons_.class).data(complaintReasons)
                        .where(b -> b.ID().eq(complaintReasons.getId()));
                db.run(update);
                complaintReasons.setIsInActive(true);

                sMessageKey = MessageKeys.DEACTIVATION_COMPLAINT_REASON_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.DEACTIVATE_COMPLAINT_REASON_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintReasons);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_REASON_HANDLER, "onDeactivateComplaintReasons");

    }
    /**
     * After deactivate event of perform validations/set Audit Data
     *
     * @param {@link DeactivateComplaintReasonsContext} context
     *
     * @public
     */
    @After(entity = ComplaintReasons_.CDS_NAME)
    public void afterDeactivateComplaintReasons(final DeactivateComplaintReasonsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_REASON_HANDLER,"afterDeactivateComplaintReasons");
        CqnSelect select = context.getCqn();
        ComplaintReasons complaintReasons = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasons.class);
        handler.logUpsert(Action.UPDATE, complaintReasons);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_REASON_HANDLER,"afterDeactivateComplaintReasons");
    }
}