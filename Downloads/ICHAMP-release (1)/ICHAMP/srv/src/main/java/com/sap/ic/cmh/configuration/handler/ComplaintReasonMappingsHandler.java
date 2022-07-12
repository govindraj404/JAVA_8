package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintReasonMappings_;
import cds.gen.configurationservice.ComplaintReasonMappings;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ComplaintReasonMappingsService;
import com.sap.ic.cmh.configuration.validations.ComplaintReasonMappingsValidation;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import java.util.Optional;
import java.util.stream.Stream;

@Component
@ServiceName("ConfigurationService")
public class ComplaintReasonMappingsHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(ComplaintReasonMappingsHandler.class);
    private static final String COMPLAINT_REASON_MAPPINGS_HANDLER = "ComplaintReasonMappingsHandler";

    @Autowired
    ComplaintReasonMappingsValidation validation;
    @Autowired
    ComplaintReasonMappingsService service;

    @Autowired
    Messages messages;

    @Autowired
	private AuditLogHelper<ComplaintReasonMappings> auditLogHelper;
    /**
     * Before Create event of ComplaintReasonMappingsHandler perform validations
     *
     * @param {@link ComplaintReasonMappings} complaintReasonMappings
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = ComplaintReasonMappings_.CDS_NAME)
    public void beforeCreateComplaintReasonMap(ComplaintReasonMappings complaintReasonMappings) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "beforeCreateComplaintReasonMap");
        logger.info("Inside ComplaintReasonMappingsHandler:beforeCreateComplaintReasonMap");
        validation.validateComplaintReasonMapExist(complaintReasonMappings);
        messages.throwIfError();
        logger.info("Completed execution of ComplaintReasonMappingsHandler:beforeCreateComplaintReasonMap");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "beforeCreateComplaintReasonMap");

    }

    /**
     * Before Update event of ComplaintReasonMappingsHandler perform validations
     *
     * @param {@link ComplaintReasonMappings} complaintReasonMappings
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = ComplaintReasonMappings_.CDS_NAME)
    public void beforeUpdateComplaintReasonMap(ComplaintReasonMappings complaintReasonMappings) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "beforeUpdateComplaintReasonMap");
        logger.info("Inside ComplaintReasonMappingsHandler:beforeUpdateComplaintReasonMap");
        validation.validateComplaintReasonMapExist(complaintReasonMappings);
        messages.throwIfError();
        setOldAuditData(complaintReasonMappings);
        logger.info("Completed execution of ComplaintReasonMappingsHandler:beforeUpdateComplaintReasonMap");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "beforeUpdateComplaintReasonMap");
    }

    /**
     * On create event
     *
     * @param {@link ComplaintReasonMappings} complaintReasonMappings
     *
     * @public
     */
    @On(event = {CdsService.EVENT_CREATE}, entity = ComplaintReasonMappings_.CDS_NAME)
    public void onCreateComplaintReasonMap(ComplaintReasonMappings complaintReasonMappings) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "onCreateComplaintReasonMap");
        logger.info("Inside ComplaintReasonMappingsHandler:onCreateComplaintReasonMap");
        Optional<Row> mapFirst= service.getComplaintReasonMapIdentifier().first();
        Integer sequenceNumber = (mapFirst.isPresent()&&null!=mapFirst.get().get("identifier")) ? Integer.parseInt(mapFirst.get().get("identifier").toString()) + 1 : 1;
        complaintReasonMappings.setIdentifier(sequenceNumber);
        logger.info("Inside ComplaintReasonMappingsHandler:onCreateComplaintReasonMap");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "onCreateComplaintReasonMap");
    }

    @After(event = CdsService.EVENT_READ, entity = ComplaintReasonMappings_.CDS_NAME)
    public void afterComplaintReasonMapping(CdsReadEventContext context, Stream<ComplaintReasonMappings> compRsnType) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER,"afterComplaintReasonMapping");
        compRsnType.forEach(b -> {
                b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER, "afterComplaintReasonMapping");

    }
    /**
     * After Update event of ComplaintReasonMappingsHandler perform validations
     *
     * @param {@link ComplaintReasonMappings} compReasonMapType
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ComplaintReasonMappings_.CDS_NAME)
	public void afterUpdateComplaintReasonMap(ComplaintReasonMappings compReasonMapType) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER,"afterUpdateComplaintReasonMap");
		logUpsert(Action.UPDATE, compReasonMapType);
		LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER,"afterUpdateComplaintReasonMap");
	}
    /**
     * After Create event of ComplaintReasonMappingsHandler perform validations
     * @param {@link ComplaintReasonMappings} compReasonMapType
     * @public
     */
	@After(event = {CdsService.EVENT_CREATE}, entity = ComplaintReasonMappings_.CDS_NAME)
	public void afterCreateComplaintReasonMap(ComplaintReasonMappings compReasonMapType) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_HANDLER,"afterCreateComplaintReasonMap");
		logUpsert(Action.CREATE, compReasonMapType);
		LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_MAPPINGS_HANDLER,"afterCreateComplaintReasonMap");
	}
    /**
     * @param action
     * @param newData
     */
	public void logUpsert(Action action, ComplaintReasonMappings newData) {
		auditLogHelper.logConfigChange(ComplaintReasonMappings_.CDS_NAME, action, newData);
	}
    /**
     * Performs setting old Auditlog Data
     * @param compReasonMapType
     */
	public void setOldAuditData(ComplaintReasonMappings compReasonMapType) {
		ComplaintReasonMappings oldData = service.getComplaintReasonMappingsDetails(compReasonMapType.getId());
		auditLogHelper.setOldData(oldData);
	}
}
