package com.sap.ic.cmh.utils.datavalidation;

import cds.gen.sap.common.Currencies;
import com.sap.cds.Result;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.util.function.Function;


@Component
public class DataValidatorImpl implements DataValidator {
    public static final Logger logger = LoggerHelper.getLogger(DataValidatorImpl.class);

    @Autowired
    private SecurityValidator securityValidator;

    /* MasterDataServiceHelper instance */
    @Autowired
    private MasterDataDao masterDataDao;

    @Autowired
    private Messages messages;

    /**
     * This method is used to validate the email address.
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateEmail(String email, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateEmailImpl(email, message, targetClass, targetClassAttribute, false);
    }

    @Override
    public <E extends StructuredType<E>> void validateEmail(String email, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateEmailImpl(email, message, targetClass, targetClassAttribute, isMandatory);
    }

    public <E extends StructuredType<E>> void validateEmailImpl(String email, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateEmail");
        if (isMandatory) {
            if (email == null) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            } else {
                if (!isEmail(email)) {
                    messages.error(message).target("in", targetClass, targetClassAttribute);
                }
            }
        } else {
            if (email != null && !isEmail(email)) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            }
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateEmail");
    }

    /**
     * This method is used to validate the phone numbers.
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateMobile(String mobile, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateMobileImpl(mobile, message, targetClass, targetClassAttribute, false);
    }

    @Override
    public <E extends StructuredType<E>> void validateMobile(String mobile, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateMobileImpl(mobile, message, targetClass, targetClassAttribute, isMandatory);
    }

    public <E extends StructuredType<E>> void validateMobileImpl(String mobile, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validatePhone");
        if (isMandatory) {
            if (mobile == null) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            } else {
                if (!isMobile(mobile)) {
                    messages.error(message).target("in", targetClass, targetClassAttribute);
                }
            }
        } else {
            if (mobile != null && !isMobile(mobile)) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            }
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validatePhone");
    }

    /**
     * This method is used to validate the data which accepts only numbers.
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateNumber(String number, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateNumberImpl(number, message, targetClass, targetClassAttribute, false);
    }

    @Override
    public <E extends StructuredType<E>> void validateNumber(String number, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateNumberImpl(number, message, targetClass, targetClassAttribute, isMandatory);
    }

    public <E extends StructuredType<E>> void validateNumberImpl(String number, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateNumber");
        if (isMandatory) {
            if (number == null) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            } else {
                if (!isNumber(number)) {
                    messages.error(message).target("in", targetClass, targetClassAttribute);
                }
            }
        } else {
            if (number != null && !isNumber(number)) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            }
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateNumber");
    }

    /**
     * This method is used to validate the data which accepts any characters.
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateDataImpl(data, message, targetClass, targetClassAttribute, false, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        validateDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
    }

    public <E extends StructuredType<E>> void validateDataImpl(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateData");
        if (isMandatory && ObjectUtils.isEmpty(data)) {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        if (isSanitizationRequired && !ObjectUtils.isEmpty(data) && !securityValidator.isValidText(data)) {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateData");
    }

    /**
     * Method used to validate alpha numeric with restriction of few special characters
     *
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateDataWithSpecialChar(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired, String regExPattern) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateDataWithSpecialChar");
        validateDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
        if (!isMatchesRegex(data,regExPattern)) {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateDataWithSpecialChar");
    }

    /**
     * This method is used to validate the data which only accepts alphabets
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateAlphabeticDataImpl(data, message, targetClass, targetClassAttribute, false, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateAlphabeticDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        validateAlphabeticDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
    }

    public <E extends StructuredType<E>> void validateAlphabeticDataImpl(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateAlphabeticDataImpl");
        validateDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
        if (!isAlphabet(data)) {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateAlphabeticDataImpl");
    }

    /**
     * This method is used to validate the data which accepts alphabets and numbers
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateAlphaNumericDataImpl(data, message, targetClass, targetClassAttribute, false, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateAlphaNumericDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, true);
    }

    @Override
    public <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        validateAlphaNumericDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
    }

    public <E extends StructuredType<E>> void validateAlphaNumericDataImpl(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateAlphaNumericData");
        validateDataImpl(data, message, targetClass, targetClassAttribute, isMandatory, isSanitizationRequired);
        if (!isAlphanumeric(data)) {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateAlphaNumericData");
    }
    
    /**
     * This method is used to validate the Currencies
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateCurrencies(String currencyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateCurrenciesImpl(currencyCode, message, targetClass, targetClassAttribute, false);
    }

    @Override
    public <E extends StructuredType<E>> void validateCurrencies(String currencyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateCurrenciesImpl(currencyCode, message, targetClass, targetClassAttribute, isMandatory);
    }

    public <E extends StructuredType<E>> void validateCurrenciesImpl(String currencyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateCurrencies");
        if (isMandatory || currencyCode != null) {
            Result aCurrencies = masterDataDao.getCurrencies(currencyCode);
            if (aCurrencies == null || !aCurrencies.first(Currencies.class).isPresent()) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            }
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateCurrencies");
    }

    /**
     * This method is used to validate the Country
     *
     */
    @Override
    public <E extends StructuredType<E>> void validateCountry(String countryCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        validateCountryImpl(countryCode, message, targetClass, targetClassAttribute, false);
    }

    @Override
    public <E extends StructuredType<E>> void validateCountry(String countryCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        validateCountryImpl(countryCode, message, targetClass, targetClassAttribute, isMandatory);
    }

    public <E extends StructuredType<E>> void validateCountryImpl(String countryCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateCountry");
        if (isMandatory || countryCode != null) {
            Result aCountries = masterDataDao.getCountryCode(countryCode);
            if (aCountries == null || !aCountries.first(Currencies.class).isPresent()) {
                messages.error(message).target("in", targetClass, targetClassAttribute);
            }
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateCountry");
    }

    private boolean isAlphabet(String str){
        return str == null || str.matches(Constants.ALPHABETS);
    }

    private boolean isAlphanumeric(String str){
        return str == null || str.matches(Constants.ALPHANUMERIC);
    }

    private boolean isNumber(String str){
        return str.matches(Constants.NUMBERS);
    }

    private boolean isMobile(String str){
        return str.matches(Constants.MOBILE);
    }

    private boolean isEmail(String str){
        return str.matches(Constants.EMAIL);
    }

    private boolean isMatchesRegex(String str,String rex){
        return str == null || str.matches(rex);
    }

}
