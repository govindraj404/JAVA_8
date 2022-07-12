package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.*;
import io.micrometer.core.instrument.util.StringUtils;

import java.util.List;
import java.util.function.Function;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.StructuredType;

import java.math.BigDecimal;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;

@Component
public class ConfigurationFieldsValidationImpl implements ConfigurationFieldsValidation {

	@Autowired
	PersistenceService db;

	@Autowired
	Messages messages;

	@Autowired
	SecurityValidator securityValidator;

	@Autowired
	ConfigurationService configurationService;

	@Autowired
	MasterDataDao masterDataDao;
	
    @Autowired
    ItemCategoriesDao itemCategoriesDao;
    
    @Autowired
    ReferenceTypeDao referenceTypeDao;

	private static final Logger logger = LoggerFactory.getLogger(ConfigurationFieldsValidationImpl.class);
	private static final String CONFIGURATION_FIELDS_VALIDATION_IMPL = "ConfigurationFieldsValidationImpl";

	/**
	 * Common method to call ComplaintTypes validation
	 *
	 * @param {@link ComplaintTypes} complaintTypes
	 *
	 * @public
	 */
	@Override
	public boolean validateComplaintType(String complaintType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateComplaintType");
		if (complaintType == null || complaintType.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateComplaintType");
		return validateComplaintTypeIfExist(complaintType);
	}

	/**
	 * Common method to call ComplaintTypes validation if exist
	 *
	 * @param {@link ComplaintTypes} complaintTypes
	 *
	 * @private
	 */
	public boolean validateComplaintTypeIfExist(String complaintType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateComplaintTypeIfExist");
		if (complaintType != null) {
			Result complaintCategories = db.run(Select.from(ComplaintCategories_.class).where(b -> b.code().eq(complaintType)));
			return (complaintCategories != null && complaintCategories.first().isPresent());
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateComplaintTypeIfExist");
		return false;
	}

	/**
	 * Common method to call BusinessObject Attribute validation
	 *
	 * @param {@link BusinessObjectAttribute} businessObjectAttribute
	 *
	 * @public
	 */
	@Override
	public boolean validateBusinessObjectAttribute(String businessObjectAttributes) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectAttribute");
		if (businessObjectAttributes == null || businessObjectAttributes.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectAttribute");
		return validateBusinessObjectAttributeIfExist(businessObjectAttributes);
	}

	/**
	 * Common method to call BusinessObject Attribute validation
	 *
	 * @param {@link BusinessObjectAttribute} businessObjectAttribute
	 *
	 * @private
	 */
	public boolean validateBusinessObjectAttributeIfExist(String businessObjectAttr) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL,
				"validateBusinessObjectAttributeIfExist");
		if (businessObjectAttr != null) {
			Result businessObjectAttributes = db.run(Select.from(BusinessObjectAttributes_.class)
					.where(b -> b.businessObjectAttribute().eq(businessObjectAttr)));
			return (businessObjectAttributes != null && businessObjectAttributes.first().isPresent());
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL,
				"validateBusinessObjectAttributeIfExist");
		return false;
	}

	/**
	 * Common method to call Destination validation
	 *
	 * @param {@link Destination} destination
	 *
	 * @public
	 */
	@Override
	public boolean validateDestinationValue(String destination) {
		List<Destinations> destinationList = configurationService.getallDestinationsFromBTP();
		return destinationList.stream().anyMatch(dest -> dest.getDestination().equals(destination));
	}

	/**
	 * Common method to call Destination validation
	 *
	 * @param {@link Destination} destination
	 *
	 * @public
	 */
	@Override
	public boolean validateDestination(String destination) {
		return !(destination == null || destination.isEmpty());
	}

	/**
	 * Common method to call BusinessObject Type validation
	 *
	 * @param {@link BusinessObjectType} businessObjectType
	 *
	 * @public
	 */
	@Override
	public boolean validateBusinessObjectType(String businessObjectType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectType");
		if (businessObjectType == null) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectType");
		return validateBusinessObjectTypeIfExist(businessObjectType);
	}

	/**
	 * Common method to call BusinessObject Type validation
	 *
	 * @param {@link BusinessObjectType} businessObjectType
	 *
	 * @private
	 */
	public boolean validateBusinessObjectTypeIfExist(String businessObjectType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectTypeIfExist");
		if (businessObjectType != null && !businessObjectType.isEmpty()) {
			Result businessObjectTypes = db
					.run(Select.from(BusinessObjectTypes_.class).where(b -> b.code().eq(businessObjectType)));
			return (businessObjectTypes != null && businessObjectTypes.first().isPresent());
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateBusinessObjectTypeIfExist");
		return false;
	}

	/**
	 * Common method to call CompanyCode validation
	 *
	 * @param {@link CompanyCode} companyCode
	 *
	 * @public
	 */
	@Override
	public boolean validateCompanyCode(String companyCodeId) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCompanyCode");
		if (companyCodeId == null || companyCodeId.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCompanyCode");
		return validateCompanyCodeIfExist(companyCodeId);
	}

	/**
	 * Common method to call CompanyCode validation if exist
	 *
	 * @param {@link CompanyCode} companyCode
	 *
	 * @private
	 */
	public boolean validateCompanyCodeIfExist(String companyCodeId) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCompanyCodeIfExist");
		if (companyCodeId != null) {
			Result companyCodes = db.run(Select.from(CompanyCodes_.class).where(b -> b.ID().eq(companyCodeId)));
			return (companyCodes != null && companyCodes.first().isPresent());
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCompanyCodeIfExist");
		return false;
	}

	/**
	 * Common method to call ItemType validation
	 *
	 * @param {@link ItemType} itemType
	 *
	 * @public
	 */
	@Override
	public boolean validateItemType(String itemType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateItemType");
		if (itemType == null || itemType.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateItemType");
		return validateItemTypeIfExist(itemType);
	}

	/**
	 * Common method to call ItemType validation
	 *
	 * @param {@link ItemType} itemType
	 *
	 * @private
	 */
	public boolean validateItemTypeIfExist(String itemType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateItemTypeIfExist");
		if (itemType != null) {
			Result itemTypes = db.run(Select.from(ItemTypes_.class).where(b -> b.code().eq(itemType)));
			return (itemTypes != null && itemTypes.first().isPresent());
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateItemTypeIfExist");
		return false;
	}

	/**
	 * Common method to call SubItemType validation
	 *
	 * @param {@link SubItemType} subItemType
	 *
	 * @public
	 */
	@Override
	public boolean validateSubItemType(String subItemType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateSubItemType");
		if (subItemType == null || subItemType.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateSubItemType");
		return validateSubItemTypeIfExist(subItemType);
	}

	/**
	 * Common method to call SubItemType validation
	 *
	 * @param {@link SubItemType} subItemType
	 *
	 * @private
	 */
	public boolean validateSubItemTypeIfExist(String subItemType) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateSubItemTypeIfExist");
		if (subItemType != null) {
			Result subItemTypes = db.run(Select.from(SubItemTypes_.class).where(b -> b.code().eq(subItemType)));
			return (subItemTypes != null && subItemTypes.first().isPresent());
		}
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateSubItemTypeIfExist");
		return false;
	}

	/**
	 * Common method to call Business Object Value validation
	 *
	 * @param {@link BusinessObjectValue} businessObjectValue
	 *
	 * @public
	 */
	public boolean validateBusinessObjectValue(String boValue) {
		return (boValue == null || boValue.isEmpty());
	}

	/**
	 * Common method to call Business Object Value validation
	 *
	 * @param {@link BusinessObjectValue} businessObjectValue
	 *
	 * @public
	 */
	public boolean validateBusinessObjectValueIfExist(String boValue) {
		return ((boValue != null) && (securityValidator.isValidText(boValue)));
	}

	@Override
	public boolean validateCurrency(String currency) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCurrency");
		if (currency == null || currency.isEmpty()) {
			return false;
		}
		LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCurrency");
		return validateCurrencyIfExist(currency);
	}

	public boolean validateCurrencyIfExist(String currencyCode) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCurrencyIfExist");
		if (StringUtils.isNotBlank(currencyCode)) {
			Result curr = masterDataDao.getCurrencies(currencyCode);
			return (curr != null && curr.first().isPresent());
		}
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateCurrencyIfExist");
		return false;
	}

    @Override
	public boolean isValidateNumericValue(BigDecimal number){
		return (number.compareTo(new BigDecimal(0))>=0);
	}
    
    /**
	 * check if item category exists 
	 * 
	 * @param {@link String} itemCategoryId
	 */
	@Override
	public boolean checkItemCategoryExist(String itemCategoryId) {
		Result itemCategoryResult = itemCategoriesDao.getComplaintItemCategory(itemCategoryId);
		return itemCategoryResult.first().isPresent();
	}

    /**
	 * validate item category 
	 * 
	 * @param {@link String, Class<E>, Function<E, Object>} 
	 * complaintItemCategoryId, targetClass, targetClassAttribute
	 * 
	 * @public
	 */

	@Override
	public <E extends StructuredType<E>> void validateItemCategory(String complaintItemCategoryId, Class<E> targetClass,
			Function<E, Object> targetClassAttribute) {
        if(complaintItemCategoryId==null){
        	messages.error(MessageKeys.ITEM_CATEGORY_IS_MANDATORY).target("in",targetClass, targetClassAttribute);
        }
        else if(Boolean.FALSE.equals(checkItemCategoryExist(complaintItemCategoryId))){
        	messages.error(MessageKeys.INVALID_ITEM_CATEGORY).target("in",targetClass, targetClassAttribute);
        } 
        LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateComplaintItemCategory");
	}
	
	/**
	 * validate reference type 
	 * 
	 * @param {@link String, Class<E>, Function<E, Object>} 
	 * referenceId, targetClass, targetClassAttribute
	 * 
	 * @public
	 */
	@Override
	public <E extends StructuredType<E>> void validateReferenceType(String referenceId, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateReferenceTye"); 
        if(referenceId==null){
        	messages.error(MessageKeys.NO_REFERENCE_TYPE_SPECIFIED).target("in",targetClass, targetClassAttribute);
        }
        else if(Boolean.FALSE.equals(checkReferenceTypeExist(referenceId))){
        	messages.error(MessageKeys.INVALID_REFERENCE_TYPE).target("in",targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodExit(logger, CONFIGURATION_FIELDS_VALIDATION_IMPL, "validateReferenceTye"); 	
	}
	
    /**
	 * check if item reference type exists 
	 * 
	 * @param {@link String} itemCategoryId
	 */
	@Override
	public boolean checkReferenceTypeExist(String referenceTypeId) {
		Result referenceTypeResult=referenceTypeDao.getReferenceTypeBasedOnId(referenceTypeId);
		return referenceTypeResult.first().isPresent();
	}
	
	
}
