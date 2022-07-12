
package com.sap.ic.cmh.configuration.handler;

import java.util.Optional;
import java.util.stream.Stream;

import com.sap.cds.services.handler.annotations.After;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.configuration.validations.TargetTypeConfigurationValidation;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.TargetTypes_;
import cds.gen.configurationservice.TargetTypes;

import com.sap.cds.services.auditlog.Action;
import com.sap.ic.cmh.auditlog.AuditLogHelper;

@Component
@ServiceName("ConfigurationService")
public class TargetTypeConfigurationHandler implements EventHandler {

    @Autowired
    TargetTypeConfigurationValidation targetTypeValidator;

    @Autowired
    Messages messages;

    @Autowired
    TargetTypeConfigurationService targetTypeService;
    @Autowired
    private AuditLogHelper<TargetTypes> auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(TargetTypeConfigurationHandler.class);
    private static final String TARGET_TYPE_HANDLER = "TargetTypeHandler";


    /**
     * Before Create event of Target Type perform validations
     *
     * @param {@link TargetTypes} targetTypes
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_CREATE}, entity = TargetTypes_.CDS_NAME)
    public void beforeTargetTypeCreateUpdate(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "beforeTargetTypeCreateUpdate");
        targetTypeValidator.validateTargetType(targetType);
        messages.throwIfError();
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "beforeTargetTypeCreateUpdate");
    }
    /**
     * Before Update event of Target Type perform validations
     *
     * @param {@link TargetTypes} targetTypes
     *
     * @public
     */
    @Before(event = {CdsService.EVENT_UPDATE}, entity = TargetTypes_.CDS_NAME)
    public void beforeTargetTypeUpdate(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "beforeTargetTypeUpdate");
        targetTypeValidator.validateTargetType(targetType);
        messages.throwIfError();
        setOldAuditData(targetType);
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "beforeTargetTypeUpdate");
    }
    /**
     *
     * After Read event of Target Type Configuration Object perform validations
     *
     * @public
     */
    @After(event = CdsService.EVENT_READ, entity = TargetTypes_.CDS_NAME)
    public void afterTargetType( Stream<TargetTypes> targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER,"afterTargetType");
        targetType.forEach(b -> {
            b.setIsInActive(true);
            if (b.getIsActiveEntity() != null && b.getHasDraftEntity() != null && b.getIsActive() != null &&
                    Boolean.TRUE.equals(b.getIsActiveEntity()) && Boolean.FALSE.equals(b.getHasDraftEntity())) {
                b.setIsInActive(!b.getIsActive());
            }
        });
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_HANDLER, "afterTargetType");

    }

    /**
     * On Create event of Target Type perform set default
     * values
     *
     * @param {@link TargetTypes} targetTypes
     *
     * @public
     */
    @On(event = {CdsService.EVENT_CREATE}, entity = TargetTypes_.CDS_NAME)
    public void onCreateTargetType(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "onCreateTargetType");
        Optional<Row> targetTypeFirst = targetTypeService.getTargetTypeConfigurations().first();
        Integer sequenceNumber = (targetTypeFirst.isPresent()&&null!=targetTypeFirst.get().get("identifier")) ? Integer.parseInt(targetTypeFirst.get().get("identifier").toString()) + 1 : 1;
        targetType.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "onCreateTargetType");
    }
    /**
     *
     * After Update event of Target Type Configuration Object perform validations
     *
     * @public
     */
    @After(event = {CdsService.EVENT_UPDATE}, entity = TargetTypes_.CDS_NAME)
    public void afterUpdateTargetType(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER,"afterUpdateTargetType");
        logUpsert(Action.UPDATE, targetType);
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_HANDLER,"afterUpdateTargetType");
    }
    /**
     *
     * After Create event of Target Type Configuration Object perform validations
     *
     * @public
     */

    @After(event = {CdsService.EVENT_CREATE}, entity = TargetTypes_.CDS_NAME)
    public void afterCreateTargetType(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER,"afterCreateTargetType");
        logUpsert(Action.CREATE, targetType);
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_HANDLER,"afterCreateTargetType");
    }
    /**
     *
     * To send old and new Target Type for triggering Audit Logs based on data
     *
     * @public
     */
    public void logUpsert(Action action, TargetTypes newData) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "logUpsert");
        auditLogHelper.logConfigChange(TargetTypes_.CDS_NAME, action, newData);
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_HANDLER,"logUpsert");
    }
    /**
     *
     * To set old data before to compare with new data for triggering audit togs
     *
     * @public
     */
    public void setOldAuditData(TargetTypes targetType) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_HANDLER, "setOldAuditData");
        TargetTypes oldData = targetTypeService.getTargetTypesDetails(targetType.getId());
        auditLogHelper.setOldData(oldData);
        LoggerHelper.logMethodExit(logger, TARGET_TYPE_HANDLER,"setOldAuditData");
    }


}
