package com.sap.ic.cmh.configuration.handler;

import java.util.Optional;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Row;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.ComplaintTypeConfigurationService;
import com.sap.ic.cmh.configuration.validations.ComplaintTypeConfigurationValidation;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ComplaintTypeConfigurations_;

@Component
@ServiceName("ConfigurationService")
public class ComplaintTypeConfigurationHandler implements EventHandler {

    @Autowired
    ComplaintTypeConfigurationValidation validator;

    @Autowired
    ComplaintTypeConfigurationService service;

    @Autowired
    Messages messages;

    @Autowired
    private AuditLogHelper<ComplaintTypeConfigurations> auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(ComplaintTypeConfigurationHandler.class);
    private static final String COMPLAINT_TYPE_CONFIG_HANDLER = "ComplaintTypeConfigurationHandler";

    /**
     * Perform input validation and unique combination check
     *
     * @param {@link ComplaintTypeConfigurations} complaintTypeConfiguration
     *
     * @public
     */
    @Before(event = { CdsService.EVENT_CREATE }, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void beforeCreateComplaintTypeConfiguration(ComplaintTypeConfigurations complaintTypeConfiguration) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeCreate");
        validator.validateCompliantTypeConfiguration(complaintTypeConfiguration);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeCreate");
    }

    /**
     * On Create event of Complaint Types perform set identifier
     *
     * @param {@link ComplaintTypeConfigurations} complaintTypeConfiguration
     *
     * @public
     */
    @On(event = { CdsService.EVENT_CREATE }, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void onCreateComplaintTypeConfiguration(ComplaintTypeConfigurations complaintTypeConfiguration) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "onComplaintTypeCreate");
        Optional<Row> complaintTypeFirst = service.getComplaintTypeConfiguration().first();
        Integer sequenceNumber = (complaintTypeFirst.isPresent() && null != complaintTypeFirst.get().get("identifier"))
                ? Integer.parseInt(complaintTypeFirst.get().get("identifier").toString()) + 1
                : 1;
        complaintTypeConfiguration.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "onComplaintTypeCreate");
    }

    /**
     * perform input validation
     *
     * @param {@link ComplaintTypeConfigurations} complaintTypeConfiguration
     *
     * @public
     */
    @Before(event = { CdsService.EVENT_UPDATE }, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void beforeUpdateComplaintTypeConfiguration(ComplaintTypeConfigurations complaintTypeConfiguration) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeUpdate");
        validator.validateCompliantTypeConfiguration(complaintTypeConfiguration);
        messages.throwIfError();
        setOldAuditData(complaintTypeConfiguration);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeUpdate");
    }

    /**
     * perform field control
     *
     * @param {@link Stream<ComplaintTypeConfigurations>} complaintTypeConfiguration
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void afterReadComplaintTypeConfiguration(Stream<ComplaintTypeConfigurations> complaintTypeConfiguration) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "afterReadComplaintTypeConfiguration");
        complaintTypeConfiguration.forEach(b -> {
            b.setIsFieldControlIndividualComplaintType(Constants.FIELD_CONTROL_MANDATORY);
            if (b.getIndividualComplaintType() != null && b.getIndividualComplaintType().equals(Boolean.TRUE)) {
                b.setIsFieldControlIndividualComplaintType(
                        Constants.FIELD_CONTROL_READ_ONLY);
            }
        });
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "afterReadComplaintTypeConfiguration");
    }

    /**
     * perform field editability
     *
     * @param {@link ComplaintTypeConfigurations} complaintTypeConfiguration
     *
     * @public
     */
    @Before(event = DraftService.EVENT_DRAFT_PATCH, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void beforeComplaintTypeConfigurationsPatch(ComplaintTypeConfigurations complaintTypeConfiguration) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeConfigurationsPatch");
        if (complaintTypeConfiguration.getIndividualComplaintType() != null
                && complaintTypeConfiguration.getIndividualComplaintType().equals(Boolean.TRUE)) {
            complaintTypeConfiguration.setItemCategoryId(null);
        }
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "beforeComplaintTypeConfigurationsPatch");
    }

    /**
     * After event read set isActive
     *
     * @param {@link Stream<ComplaintTypeConfigurations>} compType
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void afterReadComplaintTypeConfigurationsActivity(Stream<ComplaintTypeConfigurations> compType) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"afterComplaintTypeConfigurations");
        compType.forEach(b -> {
            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER, "afterComplaintTypeConfigurations");

    }

    /**
     * After Create event of ComplaintTypeConfigurations Object
     *@param {@link ComplaintTypeConfigurations} complaintType
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void afterComplaintTypeCreation(ComplaintTypeConfigurations complaintType) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"afterComplaintTypeCreation");
        logUpsert(Action.CREATE, complaintType);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"afterComplaintTypeCreation");
    }

    /**
     * After Update event of ComplaintTypeConfigurations Object
     *@param {@link ComplaintTypeConfigurations} complaintType
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = ComplaintTypeConfigurations_.CDS_NAME)
    public void afterUpdateComplaintType(ComplaintTypeConfigurations complaintType) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"afterUpdateComplaintType");
        logUpsert(Action.UPDATE, complaintType);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"afterUpdateComplaintType");
    }
    /**
     * inside Before Create event of ComplaintTypeConfigurations Object to save old data
     *@param {@link ComplaintTypeConfigurations} complaintType
     * @public
     */
    public void setOldAuditData(ComplaintTypeConfigurations complaintType) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"setOldAuditData");
        ComplaintTypeConfigurations oldComplaintType = service.getAllComplaintTypesDetails(complaintType.getId());
        auditLogHelper.setOldData(oldComplaintType);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"setOldAuditData");

    }
    /**
     * to capture audit log
     *
     * @param {@link Action} action
     * @param {@link ComplaintTypeConfigurations} newData
     *
     * @public
     */
    public void logUpsert(Action action, ComplaintTypeConfigurations newData) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"logUpsert");
        auditLogHelper.logConfigChange(ComplaintTypeConfigurations_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_HANDLER,"logUpsert");

    }
}
