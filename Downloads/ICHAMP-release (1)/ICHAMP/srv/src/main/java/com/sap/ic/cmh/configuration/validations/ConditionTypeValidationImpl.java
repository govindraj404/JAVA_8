package com.sap.ic.cmh.configuration.validations;

import com.sap.ic.cmh.utils.SecurityValidator;
import org.springframework.stereotype.Component;
import com.sap.cds.services.messages.Message.Severity;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ConditionTypes_;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.gen.MessageKeys;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;

@Component
public class ConditionTypeValidationImpl implements ConditionTypeValidation {
    
    @Autowired
    ConditionTypeDao conditionTypeDao;

    @Autowired
    Messages messages;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    SecurityValidator securityValidator;

    public void validateConditionTypes(ConditionTypes conditionType){
        if(!configurationFieldsValidation.validateDestination(conditionType.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_MANDATORY).target("in", ConditionTypes_.class,
                    ConditionTypes_::destination);
        }
        if(!configurationFieldsValidation.validateDestinationValue(conditionType.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_INVALID).target("in", ConditionTypes_.class,
            ConditionTypes_::destination);
        }
        if(conditionType.getConditionType()==null || conditionType.getConditionType().isEmpty()){
            messages.error(MessageKeys.CONDITION_TYPE_IS_MANDATORY).target("in", ConditionTypes_.class,
                    ConditionTypes_::conditionType);
        }
        if(!configurationFieldsValidation.validateItemType(conditionType.getItemTypeCode())){
            messages.error(MessageKeys.ITEM_TYPE_IS_MANDATORY).target("in", ConditionTypes_.class,
                    ConditionTypes_::itemType_code);
        }
        if(conditionType.getDescription() != null && !securityValidator.isValidText(conditionType.getDescription())){
            messages.error(MessageKeys.INVALID_CONDITION_TYPE_DESCRIPTION).target("in", ConditionTypes_.class,
                    ConditionTypes_::description);
        }
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            Result conditionTypes = conditionTypeDao.getConditionTypesBasedOnDestinationAndItemType(conditionType.getDestination(), conditionType.getItemTypeCode());
            Optional<Row> conditionTypesFirst = conditionTypes.first();
            if(conditionTypesFirst.isPresent() && !conditionType.getId().equals(conditionTypesFirst.get().get(ConditionTypes.ID).toString())) {
            messages.error(MessageKeys.ONLY_ONE_CONDITION_TYPE_PER_DESTINATION).target("in", ConditionTypes_.class,
                    ConditionTypes_::destination);
            }
        }
    }
}