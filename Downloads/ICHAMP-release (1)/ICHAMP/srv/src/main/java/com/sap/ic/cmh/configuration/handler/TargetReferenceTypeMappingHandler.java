package com.sap.ic.cmh.configuration.handler;


import java.util.Optional;
import java.util.stream.Stream;

import com.sap.cds.Row;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.targetreferencetypemappings.TargetReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.TargetReferenceTypesValidation;
import com.sap.ic.cmh.configuration.validations.targetreferencetypemappings.TargetReferenceTypeMappingValidation;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypeMappings_;

@Component
@ServiceName("ConfigurationService")
public class TargetReferenceTypeMappingHandler implements EventHandler {

    @Autowired
    TargetReferenceTypeMappingValidation targetReferenceTypeMappingValidator;

    @Autowired
    Messages messages;

    @Autowired
    TargetReferenceTypeMappingService targetReferenceTypeMappingService;

    @Autowired
    TargetReferenceTypesValidation targetReferenceTypesValidator;
    @Autowired
    private AuditLogHelper<TargetReferenceTypeMappings> auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(TargetReferenceTypeMappingHandler.class);
    private static final String TARGET_REFERENCE_TYPE_MAPPING_HANDLER = "TargetReferenceTypeMappingHandler";


    /**
     * Before Create event of Target Reference Type Mapping perform validations
     *
     * @param {@link TargetReferenceTypeMappings} targetReferenceTypeMapping
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void beforeTargetReferenceTypeMappingCreateUpdate(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "beforeTargetReferenceTypeMappingCreateUpdate");
        targetReferenceTypeMappingValidator.validateTargetReferenceTypeMapping(targetReferenceTypeMapping);
        targetReferenceTypesValidator.validateTargetReferenceTypes(targetReferenceTypeMapping);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "beforeTargetReferenceTypeMappingCreateUpdate");
    }
    /**
     * Before Update event of Target Reference Type Mapping perform validations
     *
     * @param {@link TargetReferenceTypeMappings} targetReferenceTypeMapping
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void beforeTargetReferenceTypeMappingUpdate(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "beforeTargetReferenceTypeMappingUpdate");
        targetReferenceTypeMappingValidator.validateTargetReferenceTypeMapping(targetReferenceTypeMapping);
        targetReferenceTypesValidator.validateTargetReferenceTypes(targetReferenceTypeMapping);
        messages.throwIfError();
        setOldAuditData(targetReferenceTypeMapping);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "beforeTargetReferenceTypeMappingUpdate");
    }

    /**
     * On Create event of Target Reference Type Mappings perform set default
     * values
     *
     * @param {@link TargetReferenceTypeMappings} targetReferenceTypeMapping
     *
     * @public
     */
    @On(event = {CdsService.EVENT_CREATE}, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void onCreateTargetReferenceTypeMapping(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "onCreateTargetReferenceTypeMapping");
        Optional<Row> targetReferenceTypeMappingFirst = targetReferenceTypeMappingService.getTargetReferenceTypeMappings().first();
        Integer sequenceNumber = (targetReferenceTypeMappingFirst.isPresent()&&null!=targetReferenceTypeMappingFirst.get().get("identifier")) ? Integer.parseInt(targetReferenceTypeMappingFirst.get().get("identifier").toString()) + 1 : 1;
        targetReferenceTypeMapping.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "onCreateTargetReferenceTypeMapping");
    }
    /**
     * After Read event of Target Reference Type Mappings perform set isActive
     * values
     *
     * @param {@link Stream<TargetReferenceTypeMappings>} targetRefMapping
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void afterTargetReferenceTypeMappingRead(CdsReadEventContext context, Stream<TargetReferenceTypeMappings> targetRefMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"afterTargetReferenceTypeMappingRead");
        targetRefMapping.forEach(b -> {

            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER, "afterTargetReferenceTypeMappingRead");

    }

    /**
     * After Update event of Target Reference Type Mappings perform audit log data
     * values
     *
     * @param {@link TargetReferenceTypeMappings} referenceType
     *
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void afterUpdateTargetReferenceTypeMapping(TargetReferenceTypeMappings referenceType) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"afterUpdateTargetReferenceTypeMapping");
        logUpsert(Action.UPDATE, referenceType);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"afterUpdateTargetReferenceTypeMapping");
    }
    /**
     * After Create event of Target Reference Type Mappings perform audit log data
     * values
     *
     * @param {@link TargetReferenceTypeMappings} referenceType
     *
     * @public
     */
    @After(event = {CdsService.EVENT_CREATE}, entity = TargetReferenceTypeMappings_.CDS_NAME)
    public void afterCreateTargetReferenceTypeMapping(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"afterCreateTargetReferenceTypeMapping");
        logUpsert(Action.CREATE, targetReferenceTypeMapping);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"afterCreateTargetReferenceTypeMapping");
    }
    /**
     * calling from After Edit,Update for audit log data
     * values
     *
     * @param {@link TargetReferenceTypeMappings} newData
     *
     * @public
     */
    public void logUpsert(Action action, TargetReferenceTypeMappings newData) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"logUpsert");
        auditLogHelper.logConfigChange(TargetReferenceTypeMappings_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"logUpsert");
    }
    /**
     * calling from Before Update,Activate,Deactivate for set old data
     * values
     *
     * @param {@link TargetReferenceTypeMappings} targetReferenceTypeMapping
     *
     * @public
     */
    public void setOldAuditData(TargetReferenceTypeMappings targetReferenceTypeMapping) {
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"setOldAuditData");
        TargetReferenceTypeMappings oldData = targetReferenceTypeMappingService.getRefrenceTypeMappingBasedOnID(targetReferenceTypeMapping.getId());
        auditLogHelper.setOldData(oldData);
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_HANDLER,"setOldAuditData");
    }
}