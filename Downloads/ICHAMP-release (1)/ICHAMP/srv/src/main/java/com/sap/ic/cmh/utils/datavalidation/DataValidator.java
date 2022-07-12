package com.sap.ic.cmh.utils.datavalidation;

import com.sap.cds.ql.StructuredType;

import java.util.function.Function;

public interface DataValidator {

   <E extends StructuredType<E>> void validateCountry(String countryCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateCountry(String countryCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateCurrencies(String currencyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateCurrencies(String currencyCode, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateEmail(String email, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateEmail(String email, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateMobile(String mobile, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateMobile(String mobile, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateNumber(String number, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateNumber(String number, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateData (String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateData (String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateData (String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired);

   <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateAlphabeticData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired);

   <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute);

   <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory);

   <E extends StructuredType<E>> void validateAlphaNumericData(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired);

   <E extends StructuredType<E>> void validateDataWithSpecialChar(String data, String message, Class<E> targetClass, Function<E, Object> targetClassAttribute, boolean isMandatory, boolean isSanitizationRequired, String regExPattern);
}
