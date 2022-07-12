package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.ConfigurationService_;
import cds.gen.configurationservice.ReactivateComplaintChannelsContext;
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
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class ReactiveComplaintChannelHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintChannelConfigurationHandler handler;
    public static final Logger logger = LoggerHelper.getLogger(ReactiveComplaintChannelHandler.class);
    private static final String REACTIVATE_COMPLAINT_CHANNEL_HANDLER = "ReactiveComplaintChannelHandler";

    /**
     *
     * On Activate event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @On(entity = ComplaintChannels_.CDS_NAME)
    public void reactivateComplaintChannels(final ReactivateComplaintChannelsContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintChannels complaintChannels = ((CdsService) context.getService()).run(select)
                .single(ComplaintChannels.class);
        if (Boolean.TRUE.equals(complaintChannels.getIsActiveEntity())) {
            if(Boolean.TRUE.equals(complaintChannels.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_CHANNEL_ACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintChannels);
                complaintChannels.setIsActive(true);
                CqnUpdate update = Update.entity(ComplaintChannels_.class).data(complaintChannels)
                        .where(b -> b.ID().eq(complaintChannels.getId()));
                db.run(update);
                complaintChannels.setIsInActive(false);
                sMessageKey = MessageKeys.COMPLAINT_CHANNEL_ACTIVATION_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.COMPLAINT_CHANNEL_ACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintChannels);
        context.setCompleted();
    }

    /**
     *
     * After Activate event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @After(entity = ComplaintChannels_.CDS_NAME)
    public void afterReactivateComplaintChannels(final ReactivateComplaintChannelsContext context) {
        LoggerHelper.logMethodEntry(logger, REACTIVATE_COMPLAINT_CHANNEL_HANDLER,"afterReactivateComplaintChannels");
        CqnSelect select = context.getCqn();
        ComplaintChannels complaintReasons = ((CdsService) context.getService()).run(select)
                .single(ComplaintChannels.class);
        handler.logUpsert(Action.UPDATE, complaintReasons);
        LoggerHelper.logMethodExit(logger, REACTIVATE_COMPLAINT_CHANNEL_HANDLER,"afterReactivateComplaintChannels");
    }
}