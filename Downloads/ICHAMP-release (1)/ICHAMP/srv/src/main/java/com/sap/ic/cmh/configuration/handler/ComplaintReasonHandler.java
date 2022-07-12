package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintReasons_;
import cds.gen.configurationservice.ComplaintReasons;
import com.sap.cds.Row;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ComplaintReasonsService;
import com.sap.ic.cmh.configuration.validations.ComplaintReasonsValidation;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.stream.Stream;

@Component
@ServiceName("ConfigurationService")
public class ComplaintReasonHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(ComplaintReasonHandler.class);

    private static final String COMPLAINT_REASON_HANDLER = "ComplaintReasonHandler";

    @Autowired
    Messages messages;

    @Autowired
    ComplaintReasonsValidation complaintReasonsValidation;

    @Autowired
    ComplaintReasonsService complaintReasonsService;

    @Autowired
    private AuditLogHelper<ComplaintReasons> auditLogHelper;
    /**
     * Before Create event of ComplaintReason Object perform validations
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = ComplaintReasons_.CDS_NAME)
    public void beforeComplaintReasonCreation(ComplaintReasons complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER, "beforeComplaintReasonCreation");
        logger.info("Inside ComplaintReasonHandler:beforeComplaintReasonCreation");
        complaintReasonsValidation.validateComplaintReason(complaintReasons);
        messages.throwIfError();
        logger.info("Completed execution of ComplaintReasonHandler:beforeComplaintReasonCreation");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER, "beforeComplaintReasonCreation");
    }

    /**
     * Before Update event of ComplaintReason Object perform validations
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = ComplaintReasons_.CDS_NAME)
    public void beforeComplaintReasonUpdate(ComplaintReasons complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER, "beforeComplaintReasonUpdate");
        logger.info("Inside before Complaint Reason update");
        complaintReasonsValidation.validateComplaintReason(complaintReasons);
        messages.throwIfError();
        setOldAuditData(complaintReasons);
        logger.info("Complaint Reason data updated Successfully");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER, "beforeComplaintReasonUpdate");

    }

    /**
     * On Create event of ComplaintReason Object
     *
     * @public
     */
    @On(event = CdsService.EVENT_CREATE, entity = ComplaintReasons_.CDS_NAME)
    public void onComplaintReasonCreation(ComplaintReasons complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER, "onComplaintReasonCreation");
        logger.info("Inside on Complaint Reason create");
        Optional<Row> getAllComplaintReasons = complaintReasonsService.getAllComplaintReasonsOrderByIdentifier().first();
        Integer sequenceNumber = (getAllComplaintReasons.isPresent()&&null!=getAllComplaintReasons.get()
                .get("identifier")) ? Integer.parseInt(getAllComplaintReasons.get().get("identifier")
                .toString()) + 1 : 1;
        complaintReasons.setIdentifier(sequenceNumber);
        logger.info("Complaint Reason Created Successfully");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER, "onComplaintReasonCreation");
    }
    /**
     * After Read event of ComplaintReason Object set active/inactive
     *@param {@link Stream<ComplaintReasons>} complaintReasons
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = ComplaintReasons_.CDS_NAME)
    public void afterComplaintReasonsRead(CdsReadEventContext context, Stream<ComplaintReasons> complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER,"afterComplaintReasonsRead");
        complaintReasons.forEach(b -> {

            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER, "afterComplaintReasonsRead");
    }
    /**
     * After Create event of ComplaintReason Object
     *@param {@link ComplaintReasons} complaintReasons
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = ComplaintReasons_.CDS_NAME)
    public void afterComplaintReasonCreation(ComplaintReasons complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER,"afterComplaintReasonCreation");
        logUpsert(Action.CREATE, complaintReasons);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER,"afterComplaintReasonCreation");
    }

    /**
     * After Update event of ComplaintReason Object
     *@param {@link ComplaintReasons} complaintReasons
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ComplaintReasons_.CDS_NAME)
    public void afterUpdateComplaintReason(ComplaintReasons complaintReasons) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER,"afterUpdateComplaintReason");
        logUpsert(Action.UPDATE, complaintReasons);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER,"afterUpdateComplaintReason");
    }
    /**
     * inside Before Create event of ComplaintReason Object to save old data
     *@param {@link ComplaintReasons} complaintReason
     * @public
     */
    public void setOldAuditData(ComplaintReasons complaintReason) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER,"setOldAuditData");
        ComplaintReasons oldComplaintReason = complaintReasonsService.getComplaintReasonBasedOnID(complaintReason.getId());
        auditLogHelper.setOldData(oldComplaintReason);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER,"setOldAuditData");

    }
    /**
     * to capture audit log
     *
     * @param {@link Action} action
     * @param {@link ComplaintReasons} newData
     *
     * @public
     */
    public void logUpsert(Action action, ComplaintReasons newData) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_HANDLER,"logUpsert");
        auditLogHelper.logConfigChange(ComplaintReasons_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_HANDLER,"logUpsert");

    }

}