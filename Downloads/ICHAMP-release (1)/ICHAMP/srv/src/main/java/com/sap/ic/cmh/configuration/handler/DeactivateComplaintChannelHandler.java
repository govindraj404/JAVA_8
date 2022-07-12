package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.ConfigurationService_;
import cds.gen.configurationservice.DeactivateComplaintChannelsContext;
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
public class DeactivateComplaintChannelHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ComplaintChannelConfigurationHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateComplaintChannelHandler.class);
    private static final String DEACTIVATE_COMPLAINT_CHANNEL_HANDLER = "DeactivateComplaintChannelHandler";

    /**
     *
     * On Deactivate event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @On(entity = ComplaintChannels_.CDS_NAME)
    public void deactivateComplaintChannels(final DeactivateComplaintChannelsContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ComplaintChannels complaintChannels = ((CdsService) context.getService()).run(select)
                .single(ComplaintChannels.class);
        if (Boolean.TRUE.equals(complaintChannels.getIsActiveEntity())) {
            if(Boolean.FALSE.equals(complaintChannels.getIsActive())){
                sMessageKey = MessageKeys.COMPLAINT_CHANNEL_DEACTIVATE_NOT_SUCCESSFUL;
            }
            else {
                handler.setOldAuditData(complaintChannels);
                complaintChannels.setIsActive(false);
                CqnUpdate update = Update.entity(ComplaintChannels_.class).data(complaintChannels)
                        .where(b -> b.ID().eq(complaintChannels.getId()));
                db.run(update);
                complaintChannels.setIsInActive(true);
                sMessageKey = MessageKeys.COMPLAINT_CHANNEL_DEACTIVATION_SUCCESSFUL;
            }
        } else {

            sMessageKey = MessageKeys.COMPLAINT_CHANNEL_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaintChannels);
        context.setCompleted();
    }

    /**
     *
     * After Deactivate event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @After(entity = ComplaintChannels_.CDS_NAME)
    public void afterDeactivateComplaintChannels(final DeactivateComplaintChannelsContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_COMPLAINT_CHANNEL_HANDLER,"afterDeactivateComplaintChannels");
        CqnSelect select = context.getCqn();
        ComplaintChannels complaintChannels = ((CdsService) context.getService()).run(select)
                .single(ComplaintChannels.class);
        handler.logUpsert(Action.UPDATE, complaintChannels);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_COMPLAINT_CHANNEL_HANDLER,"afterDeactivateComplaintChannels");
    }
}