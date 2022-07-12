package com.sap.ic.cmh.costcollector.validations;

import java.math.BigDecimal;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.costcollectorservice.CostCollectors_;

@Component
public class CostCollectorValidationImpl implements CostCollectorValidation {

    @Autowired
    CostCollectorDao costCollectorDao;

    @Autowired
    Messages messages;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    MasterDataValidation masterDataValidation;

    @Override
    public void validateCostCollectorFRAttributes(CostCollectors costCollector) {

        if (!configurationFieldsValidation.validateSubItemType(costCollector.getSubItemTypeCode())) {
            messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY).target("in", CostCollectors_.class,
                    CostCollectors_::subItemType_code);
        }
        if (!configurationFieldsValidation.isValidateNumericValue(costCollector.getQuantity())) {
            messages.error(MessageKeys.NUMBER_IS_NEGATIVE).target("in", CostCollectors_.class,
                    CostCollectors_::quantity);
        }
        if (StringUtils.isNotBlank(costCollector.getUnitCode())) {
            masterDataValidation.validUnitOfMeasure(costCollector.getUnitCode());
        }
    }

    @Override
    public void validateCostCollectorSUBLAttributes(CostCollectors costCollector) {

        if (!configurationFieldsValidation.validateSubItemType(costCollector.getSubItemTypeCode())) {
            messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY).target("in", CostCollectors_.class,
                    CostCollectors_::subItemType_code);
        }
        if (!configurationFieldsValidation.isValidateNumericValue(costCollector.getTotalCost())) {
            messages.error(MessageKeys.NUMBER_IS_NEGATIVE).target("in", CostCollectors_.class,
                    CostCollectors_::totalCost);
        }
        if(!validateTotalCost(costCollector.getTotalCost())){
            messages.error(MessageKeys.NUMBER_LENGTH_EXCEEDED).target("in", CostCollectors_.class,
                    CostCollectors_::totalCost);
        }
    }

    protected boolean validateTotalCost(BigDecimal totalCost){
        String str = totalCost.toString();
        int wholeNumberLength = str.split("\\.")[0].length();
        return (wholeNumberLength <= Constants.MAX_LENGTH);
    }
}