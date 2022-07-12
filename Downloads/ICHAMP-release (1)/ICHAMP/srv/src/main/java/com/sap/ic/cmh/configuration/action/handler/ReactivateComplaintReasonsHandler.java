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
public class ReactivateComplaintReasonsHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintReasonHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(ReactivateComplaintReasonsHandler.class);

    private static final String REACTIVATE_COMPLAINT_REASON_HANDLER = "ReactivateComplaintReasonsHandler";

     /**
     * On reactivate event of reactivation
     *
     * @param {@link ReactivateComplaintReasonsContext} context
     *
     * @public
     */
    @On(entity = ComplaintReasons_.CDS_NAME)
    public void onReactivateComplaintReasons(final ReactivateComplaintReasonsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_REASON_HANDLER,"onReactivateComplaintReasons");

        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintReasons complaintReasons = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasons.class);
        if (Boolean.TRUE.equals(complaintReasons.getIsActiveEntity())) {
            if(Boolean.TRUE.equals(complaintReasons.getIsActive())){
                sMessageKey = MessageKeys.ACTIVATE_COMPLAINT_REASON_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintReasons);
                complaintReasons.setIsActive(true);
                CqnUpdate update = Update.entity(ComplaintReasons_.class).data(complaintReasons)
                        .where(b -> b.ID().eq(complaintReasons.getId()));
                db.run(update);
                complaintReasons.setIsInActive(false);
                sMessageKey = MessageKeys.ACTIVATION_COMPLAINT_REASON_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.ACTIVATE_COMPLAINT_REASON_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintReasons);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_REASON_HANDLER, "onReactivateComplaintReasons");

    }
    /**
     * After reactivate event of capture audit log
     *
     * @param {@link ReactivateComplaintReasonsContext} context
     *
     * @public
     */
    @After(entity = ComplaintReasons_.CDS_NAME)
    public void afterReactivateComplaintReasons(final ReactivateComplaintReasonsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_REASON_HANDLER,"afterReactivateComplaintReasons");
        CqnSelect select = context.getCqn();
        ComplaintReasons complaintReasons = ((CdsService) context.getService()).run(select)
                .single(ComplaintReasons.class);
        handler.logUpsert(Action.UPDATE, complaintReasons);
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_REASON_HANDLER,"afterReactivateComplaintReasons");
    }
}