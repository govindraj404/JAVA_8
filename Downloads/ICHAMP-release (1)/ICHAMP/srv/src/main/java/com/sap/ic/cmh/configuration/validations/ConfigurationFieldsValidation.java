package com.sap.ic.cmh.configuration.validations;

import java.math.BigDecimal;
import java.util.function.Function;
import com.sap.cds.ql.StructuredType;

public interface ConfigurationFieldsValidation {
    boolean validateComplaintType(String complaintType);

    boolean validateBusinessObjectAttribute(String businessObjectAttributes);

    boolean validateBusinessObjectType(String businessObjectType);

    boolean validateCompanyCode(String companyCode);

    boolean validateItemType(String itemType);

    boolean validateSubItemType(String subItemType);

    boolean validateBusinessObjectValue(String businessObjectValue);

    boolean validateBusinessObjectValueIfExist(String businessObjectValue);

    boolean validateDestination(String destination);

    boolean validateDestinationValue(String destination);

    boolean validateCurrency(String currency);

    boolean isValidateNumericValue(BigDecimal number);
    
    public boolean checkItemCategoryExist(String itemCategoryId);
    
    public boolean checkReferenceTypeExist(String referenceTypeId);
    
    public <E extends StructuredType<E>> void validateItemCategory(String complaintItemCategoryId, Class<E> targetClass, Function<E, Object> targetClassAttribute);
    
    public <E extends StructuredType<E>> void validateReferenceType(String referenceId, Class<E> targetClass, Function<E, Object> targetClassAttribute);
}
