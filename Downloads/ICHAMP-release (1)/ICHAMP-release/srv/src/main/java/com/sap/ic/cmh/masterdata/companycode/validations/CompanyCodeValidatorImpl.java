package com.sap.ic.cmh.masterdata.companycode.validations;

import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CompanyCodeValidatorImpl implements CompanyCodeValidator {

    public static final Logger logger = LoggerHelper.getLogger(CompanyCodeValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the given the comapny code details
     * @param companyCode
     */
    @Override
    public void checkInputsSanitized(CompanyCodes companyCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Company Code */
        dataValidator.validateData(companyCode.getCompanyCode(),
                MessageKeys.COMPANY_CODE_VALIDATION_ERROR, CompanyCodes_.class, CompanyCodes_::companyCode, true);

        /* Company Code Name */
        dataValidator.validateData(companyCode.getCompanyCodeName(),
                MessageKeys.COMPANY_CODE_NAME_VALIDATION_ERROR, CompanyCodes_.class, CompanyCodes_::companyCodeName);

        /* Address */
        dataValidator.validateData(companyCode.getAddress(),
                MessageKeys.ADDRESS_VALIDATION_ERROR, CompanyCodes_.class, CompanyCodes_::address);

        /* Country Key Code */
        dataValidator.validateCountry(companyCode.getCountryKeyCode(),
                MessageKeys.INVALID_COUNTRY_KEY_CODE, CompanyCodes_.class, CompanyCodes_::countryKey_code);

        /* Currency Code */
        dataValidator.validateCurrencies(companyCode.getCurrencyCode(),
                MessageKeys.INVALID_CURRENCY_CODE, CompanyCodes_.class, CompanyCodes_::currency_code);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}
