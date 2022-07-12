package com.sap.ic.cmh.masterdata.reason.validation;

import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.masterdataservice.Reasons;
import cds.gen.masterdataservice.Reasons_;

@Component
public class RejectReasonValidatorImpl implements RejectReasonValidator {
    public static final Logger logger = LoggerHelper.getLogger(RejectReasonValidatorImpl.class);
    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the reasons given details
     * @param reasons
     */
    @Override
    public void checkInputsSanitized(Reasons reasons) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Code */
        dataValidator.validateData(reasons.getCode(),
                MessageKeys.REASONS_CODE_VALIDATION_ERROR, Reasons_.class, Reasons_::code, true);

        /* Description */
        dataValidator.validateData(reasons.getDescription(),
                MessageKeys.REASONS_DESCRIPTION_VALIDATION_ERROR, Reasons_.class, Reasons_::description);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}

