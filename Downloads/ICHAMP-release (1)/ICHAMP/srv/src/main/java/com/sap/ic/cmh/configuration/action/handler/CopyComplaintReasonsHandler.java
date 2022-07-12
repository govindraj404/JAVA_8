package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintReasons;
import cds.gen.configurationservice.ComplaintReasons_;
import cds.gen.configurationservice.CopyComplaintReasonsContext;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@ServiceName("ConfigurationService")
public class CopyComplaintReasonsHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopyComplaintReasonsHandler.class);
    private static final String COPY_COMPLAINT_REASON = "CopyComplaintReasonsHandler";

    @Autowired
    Messages msg;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    @On(entity = ComplaintReasons_.CDS_NAME)
    public void copyComplaintReason(final CopyComplaintReasonsContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_COMPLAINT_REASON, "copyComplaintReason");
        CqnSelect select = context.getCqn();
        ComplaintReasons complaintReasons =
                ((CdsService) context.getService()).run(select)
                        .single(ComplaintReasons.class);
        logger.info("Copy Complaint reason Struct create starting");
        ComplaintReasons copyComplaintReasons = Struct.create(ComplaintReasons.class);

        copyComplaintReasons.setCode(complaintReasons.getCode());
        copyComplaintReasons.setDescription(complaintReasons.getDescription());
        logger.info("Copy Complaint reason setting result into context");

        context.setResult(
                draftService.newDraft(Insert.into(ComplaintReasons_.class).entry(copyComplaintReasons))
                        .single(ComplaintReasons.class));

        msg.success(MessageKeys.COMPLAINT_REASON_OBJECT_COPIED, ComplaintReasons_.class);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_COMPLAINT_REASON, "copyComplaintReason");
    }
}
