package com.sap.ic.cmh.masterdata.purchasinggroup.validation;

import cds.gen.masterdataservice.PurchasingGroups;
import cds.gen.masterdataservice.PurchasingGroups_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class PurchasingGroupValidatorImpl implements PurchasingGroupValidator {
    public static final Logger logger = LoggerHelper.getLogger(PurchasingGroupValidatorImpl.class);
    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the purchasingGroups given details
     * @param purchasingGroups
     */
    @Override
    public void checkInputsSanitized(PurchasingGroups purchasingGroups) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Code */
        dataValidator.validateData(purchasingGroups.getCode(),
                MessageKeys.PURCHASING_GROUP_CODE_VALIDATION_ERROR, PurchasingGroups_.class, PurchasingGroups_::code, true);

        /* Description */
        dataValidator.validateData(purchasingGroups.getDescription(),
                MessageKeys.PURCHASING_GROUP_DESCRIPTION_VALIDATION_ERROR, PurchasingGroups_.class, PurchasingGroups_::description);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}