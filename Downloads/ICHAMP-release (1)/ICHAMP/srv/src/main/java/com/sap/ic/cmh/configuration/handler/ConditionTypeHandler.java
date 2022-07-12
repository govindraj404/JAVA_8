package com.sap.ic.cmh.configuration.handler;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.sap.cds.Row;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.validations.ConditionTypeValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ConditionTypes_;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.configuration.service.ConfigurationService;

@Component
@ServiceName("ConfigurationService")
public class ConditionTypeHandler implements EventHandler {

    @Autowired
    ConditionTypeDao conditionTypeDao;

    @Autowired
    ConditionTypeValidation validator;

    @Autowired
    Messages messages;

    @Autowired
    private AuditLogHelper auditLogHelper;
    @Autowired
    private CqnAnalyzerUtil cqnAnalyzerUtil;
    @Autowired
    private AuditLogDifference<ConditionTypes> auditLogDifference;

    @Autowired
    ConfigurationService configurationService;
    
    private static final Logger logger = LoggerFactory.getLogger(ConditionTypeHandler.class);
    private static final String CONDITION_TYPE_HANDLER = "ConditionTypeHandler";
    private static final String ENTITY_NAME = "ConditionType";

    /**
	 * Before Create event of Condition Types perform validations
	 * 
	 * @param {@link ConditionTypes} conditionTypes
	 * 
	 * @public
	 */
    @Before(event = {CdsService.EVENT_CREATE,CdsService.EVENT_UPDATE}, entity = ConditionTypes_.CDS_NAME)
    public void beforeConditionTypesCreateUpdate(ConditionTypes conditionType) {
    	LoggerHelper.logMethodEntry(logger, CONDITION_TYPE_HANDLER, "beforeConditionTypesCreateUpdate");
        validator.validateConditionTypes(conditionType);
        messages.throwIfError();
        LoggerHelper.logMethodEntry(logger, CONDITION_TYPE_HANDLER, "beforeConditionTypesCreateUpdate");
    }

    /**
	 * On Create event of Condition Types perform set default
	 * values
	 * 
	 * @param {@link ConditionTypes} conditionTypes
	 * 
	 * @public
	 */
    @On(event = {CdsService.EVENT_CREATE}, entity = ConditionTypes_.CDS_NAME)
    public void onCreateConditionTypes(ConditionTypes conditionType) {
        LoggerHelper.logMethodEntry(logger, CONDITION_TYPE_HANDLER, "onConditionTypesCreate");
        Optional<Row> conditionTypesFirst = conditionTypeDao.getConditionTypes().first();
        Integer sequenceNumber = (conditionTypesFirst.isPresent()&&null!=conditionTypesFirst.get().get("identifier")) ? Integer.parseInt(conditionTypesFirst.get().get("identifier").toString()) + 1 : 1;
        conditionType.setIdentifier(sequenceNumber);
        LoggerHelper.logMethodEntry(logger, CONDITION_TYPE_HANDLER, "onConditionTypesCreate");
    }

    @Before(event = {  CdsService.EVENT_UPDATE }, entity = ConditionTypes_.CDS_NAME)
    public void beforeConditionTypeUpdate(ConditionTypes conditionType){
        setOldAuditData(conditionType);
    }
    

    @After(event = { CdsService.EVENT_CREATE, CdsService.EVENT_UPDATE }, entity = ConditionTypes_.CDS_NAME )
    public void afterConditionTypeCreateUpdate(ConditionTypes data){
        logUpsert(data);
    }

    @After(event = CdsService.EVENT_DELETE, entity = ConditionTypes_.CDS_NAME)
    public void afterConditionTypeDeletion(CdsDeleteEventContext context ) {
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        logDelete((String)keys.get(ConditionTypes.ID));
    }

    public void setOldAuditData(ConditionTypes conditionType) {
        ConditionTypes oldData = configurationService.getConditionTypeDetail(conditionType.getId());
        auditLogDifference.setOldData(oldData);
    } 
    
    public void logUpsert(ConditionTypes conditionType) {
        ConditionTypes newData = configurationService.getConditionTypeDetail(conditionType.getId());
        List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
        
        Map<String, String> entityInfoMap =
                auditLogHelper.buildEntityInfoMap(
                        ENTITY_NAME,
                        newData.getId());

        auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
    }

    public void logDelete(String id) {
        ObjectDiff diffList = null;
        Map<String, String> entityInfoMap =
                auditLogHelper.buildEntityInfoMap(
                        ENTITY_NAME, id);
        auditLogHelper.logDeleteAuditData(diffList, entityInfoMap);
    }
    

}
