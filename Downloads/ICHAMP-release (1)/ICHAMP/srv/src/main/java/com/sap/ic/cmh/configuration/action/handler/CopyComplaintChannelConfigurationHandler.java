package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.ComplaintChannels_;

import cds.gen.configurationservice.CopyComplaintChannelsContext;
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
public class CopyComplaintChannelConfigurationHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(CopyComplaintChannelConfigurationHandler.class);
    private static final String COPY_COMPLAINT_CHANNEL_CONFIG = "CopyComplaintChannelConfigurationHandler";

    @Autowired
    Messages msg;

    @Qualifier("ConfigurationService")
    @Autowired
    DraftService draftService;

    @On(entity = ComplaintChannels_.CDS_NAME)
    public void copyComplaintChannels(final CopyComplaintChannelsContext context) {
        LoggerHelper.logMethodEntry(logger, COPY_COMPLAINT_CHANNEL_CONFIG, "copyComplaintChannels");
        CqnSelect select = context.getCqn();
        ComplaintChannels complaintChannels =
                ((CdsService) context.getService()).run(select)
                        .single(ComplaintChannels.class);
        logger.info("Copy Complaint Channel Struct create starting");
        ComplaintChannels copyComplaintChannels = Struct.create(ComplaintChannels.class);

        copyComplaintChannels.setCode(complaintChannels.getCode());
        copyComplaintChannels.setDescription(complaintChannels.getDescription());
        logger.info("Copy Complaint Channel setting result into context");

        context.setResult(
                draftService.newDraft(Insert.into(ComplaintChannels_.class).entry(copyComplaintChannels))
                        .single(ComplaintChannels.class));

        msg.success(MessageKeys.COMPLAINT_CHANNEL_MESSAGE_COPIED, ComplaintChannels_.class);
        context.setCompleted();
        LoggerHelper.logMethodExit(logger, COPY_COMPLAINT_CHANNEL_CONFIG, "copyComplaintChannels");
    }

}
