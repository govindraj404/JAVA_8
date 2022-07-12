package com.sap.ic.cmh.configuration.handler;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.ServiceName;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.service.sourcereferencetypemappings.SourceReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.sourcereferencetypemappings.SourceReferenceTypeMappingValidation;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.ReferenceTypes;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import cds.gen.configurationservice.SourceReferenceTypeMappings_;
import com.sap.cds.services.auditlog.Action;
import com.sap.ic.cmh.auditlog.AuditLogHelper;

@Component
@ServiceName("ConfigurationService")
public class SourceReferenceTypeMappingHandler implements EventHandler {

    @Autowired
    SourceReferenceTypeMappingValidation sourceReferenceTypeMappingValidator;

    @Autowired
    Messages messages;

    @Autowired
    SourceReferenceTypeMappingService sourceReferenceTypeMappingService;
    @Autowired
    private AuditLogHelper<SourceReferenceTypeMappings> auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(SourceReferenceTypeMappingHandler.class);
    private static final String SOURCE_REFERENCE_TYPE_MAPPING_HANDLER = "SourceReferenceTypeMappingHandler";


    /**
     * Before Create event of Source Reference Type Mapping perform validations
     *
     * @param {@link SourceReferenceTypeMappings} sourceReferenceTypeMapping
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void beforeSourceReferenceTypeMappingCreateUpdate(SourceReferenceTypeMappings sourceReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingCreateUpdate");
        sourceReferenceTypeMappingValidator.validateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
        messages.throwIfError();
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingCreateUpdate");
    }
    /**
     * Before Update event of Source Reference Type Mapping perform validations
     *
     * @param {@link TargetTypes} targetTypes
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void beforeSourceReferenceTypeMappingUpdate(SourceReferenceTypeMappings sourceReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingUpdate");
        sourceReferenceTypeMappingValidator.validateSourceReferenceTypeMapping(sourceReferenceTypeMapping);
        messages.throwIfError();
        setOldAuditData(sourceReferenceTypeMapping);
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingUpdate");
    }

    /**
     * On Create event of Source Reference Type Mappings perform set default
     * values
     *
     * @param {@link SourceReferenceTypeMappings} sourceReferenceTypeMapping
     *
     * @public
     */
    @On(event = {CdsService.EVENT_CREATE}, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void onCreateSourceReferenceTypeMapping(SourceReferenceTypeMappings sourceReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "onCreateSourceReferenceTypeMapping");
        Optional<Row> sourceReferenceTypeMappingFirst = sourceReferenceTypeMappingService.getSourceReferenceTypeMappings().first();
        Integer sequenceNumber = (sourceReferenceTypeMappingFirst.isPresent()&&null!=sourceReferenceTypeMappingFirst.get().get("identifier")) ? Integer.parseInt(sourceReferenceTypeMappingFirst.get().get("identifier").toString()) + 1 : 1;
        sourceReferenceTypeMapping.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "onCreateSourceReferenceTypeMapping");
    }

    /**
     * perform field control
     *
     * @param {@link Stream<SourceReferenceTypeMappings>} sourceReferenceTypeMapping
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void afterReadReferenceTypes(Stream<SourceReferenceTypeMappings> sourceReferenceTypeMappings) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "afterReadReferenceTypes");
        sourceReferenceTypeMappings.forEach(b -> {
            b.setIsFieldControlReferenceType(Constants.FIELD_CONTROL_READ_ONLY);
            if (b.getComplaintTypeId() != null && b.getItemCategoryId()!=null) {
                b.setIsFieldControlReferenceType(
                        Constants.FIELD_CONTROL_MANDATORY);
            }
        });
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "afterReadReferenceTypes");
    }

    /**
     * perform field editability
     *
     * @param {@link SourceReferenceTypeMappings} sourceReferenceTypeMapping
     *
     * @public
     */
    @Before(event = DraftService.EVENT_DRAFT_PATCH, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void beforeSourceReferenceTypeMappingsPatch(SourceReferenceTypeMappings sourceReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingsPatch");
        if (sourceReferenceTypeMapping.getComplaintTypeId() != null
                || sourceReferenceTypeMapping.getItemCategoryId()!=null || sourceReferenceTypeMapping.getComplaintTypeId() == null
                || sourceReferenceTypeMapping.getItemCategoryId()==null) {
            List<ReferenceTypes> refTypeList = new ArrayList<>();
            sourceReferenceTypeMapping.setReferenceTypes(refTypeList);
        }
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "beforeSourceReferenceTypeMappingsPatch");
    }
    /**
     *
     * After Read event of SourceReferenceTypeMappings Object perform validations
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void afterReadSourceReferenceTypeMapping(Stream<SourceReferenceTypeMappings> sourceReferenceTypeMappings) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "afterReadSourceReferenceTypeMapping");
        sourceReferenceTypeMappings.forEach(b -> {
            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "afterReadSourceReferenceTypeMapping");
    }
    /**
     *
     * After Update event of TSourceReferenceTypeMappings Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void afterUpdateSourceReferenceTypeMapping(SourceReferenceTypeMappings sourceRefTypeMap) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterUpdateSourceReferenceTypeMapping");
        logUpsert(Action.UPDATE, sourceRefTypeMap);
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterUpdateSourceReferenceTypeMapping");
    }
    /**
     *
     * After Create event of TSourceReferenceTypeMappings Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = SourceReferenceTypeMappings_.CDS_NAME)
    public void afterCreateSourceReferenceTypeMapping(SourceReferenceTypeMappings sourceRefTypeMap) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterCreateSourceReferenceTypeMapping");
        logUpsert(Action.CREATE, sourceRefTypeMap);
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"afterCreateSourceReferenceTypeMapping");
    }
    /**
     *
     * To send old and new SourceReferenceTypeMappings for triggering Audit Logs based on data
     *
     * @public
     */
    public void logUpsert(Action action, SourceReferenceTypeMappings newData) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "logUpsert");
        auditLogHelper.logConfigChange(SourceReferenceTypeMappings_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER,"logUpsert");
    }
    /**
     *
     * To set old data before to compare with new data for triggering audit togs
     *
     * @public
     */
    public void setOldAuditData(SourceReferenceTypeMappings sourceRefTypeMap) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_HANDLER, "setOldAuditData");
        SourceReferenceTypeMappings oldData = sourceReferenceTypeMappingService.getSourceReferenceTypeMappingsDetails(sourceRefTypeMap.getId());
        auditLogHelper.setOldData(oldData);
    }
}