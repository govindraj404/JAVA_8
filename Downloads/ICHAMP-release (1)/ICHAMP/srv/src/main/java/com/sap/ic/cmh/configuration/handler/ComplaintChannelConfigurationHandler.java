package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.ComplaintChannels;

import com.sap.cds.Row;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ComplaintChannelService;
import com.sap.ic.cmh.configuration.validations.ComplaintChannelValidation;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.stream.Stream;

@Component
@ServiceName("ConfigurationService")
public class ComplaintChannelConfigurationHandler implements EventHandler {

    private static final Logger logger = LoggerFactory.getLogger(ServiceMaterialHandler.class);
    private static final String COMPLAINT_CHANNEL_HANDLER = "ComplaintChannelConfigurationHandler";

    @Autowired
    ComplaintChannelValidation complaintChannelsValidation;

    @Autowired
    ComplaintChannelService complaintChannelsService;

    @Autowired
    private AuditLogHelper<ComplaintChannels> auditLogHelper;

    @Autowired
    Messages messages;

    /**
     * Before Create event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = ComplaintChannels_.CDS_NAME)
    public void beforeComplaintChannelCreation(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER, "beforeComplaintChannelCreation");
        logger.info("Inside before Complaint Channels create");
        complaintChannelsValidation.validateComplaintChannel(complaintChannels);
        messages.throwIfError();
        logger.info("Complaint Channel data created Successfully");
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER, "beforeComplaintChannelCreation");
    }

    /**
     *
     * Before Update event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = ComplaintChannels_.CDS_NAME)
    public void beforeComplaintChannelUpdate(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER, "beforeComplaintChannelUpdate");
        logger.info("Inside before Complaint Channel update");
        complaintChannelsValidation.validateComplaintChannel(complaintChannels);
        messages.throwIfError();
        setOldAuditData(complaintChannels);
        logger.info("Complaint Channel data updated Successfully");
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER, "beforeComplaintChannelUpdate");

    }

    /**
     *
     * On create event of ComplaintChannelConfiguration.
     *
     * @public
     */
    @On(event = CdsService.EVENT_CREATE, entity = ComplaintChannels_.CDS_NAME)
    public void onComplaintChannelCreation(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER, "onComplaintChannelCreation");
        logger.info("Inside on Complaint Channel create");
        Optional<Row> complaintChannelsID = complaintChannelsService.getAllComplaintChannelsOrderByIdentifier().first();
        Integer sequenceNumber = (complaintChannelsID.isPresent()&&null!=complaintChannelsID.get().get("identifier")) ?
                Integer.parseInt(complaintChannelsID.get().get("identifier").toString()) + 1 : 1;
        complaintChannels.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER, "onComplaintChannelCreation");
    }

    /**
     *
     * After Read event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = ComplaintChannels_.CDS_NAME)
    public void afterComplaintChannel(Stream<ComplaintChannels> complaintChannelsStream) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER,"afterComplaintChannel");
        complaintChannelsStream.forEach(b -> {
            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER, "afterComplaintChannel");

    }

    /**
     *
     * After Update event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ComplaintChannels_.CDS_NAME)
    public void afterUpdateComplaintChannel(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER,"afterUpdateComplaintChannel");
        logUpsert(Action.UPDATE, complaintChannels);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER,"afterUpdateComplaintChannel");
    }

    /**
     *
     * After Create event of ComplaintChannelConfiguration Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = ComplaintChannels_.CDS_NAME)
    public void afterCreateComplaintChannel(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER,"afterCreateComplaintChannel");
        logUpsert(Action.CREATE, complaintChannels);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER,"afterCreateComplaintChannel");
    }

    /**
     *
     * To send old and new Complaint Channel for triggering Audit Logs based on data
     *
     * @public
     */
    public void logUpsert(Action action, ComplaintChannels newData) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER,"logUpsert");
        auditLogHelper.logConfigChange(ComplaintChannels_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER,"logUpsert");
    }

    /**
     *
     * To set old data before to compare with new data for triggering audit togs
     *
     * @public
     */
    public void setOldAuditData(ComplaintChannels complaintChannels) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_HANDLER,"setOldAuditData");
        ComplaintChannels oldData = complaintChannelsService.getComplaintChannelDetails(complaintChannels.getId());
        auditLogHelper.setOldData(oldData);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_HANDLER,"setOldAuditData");
    }

}
