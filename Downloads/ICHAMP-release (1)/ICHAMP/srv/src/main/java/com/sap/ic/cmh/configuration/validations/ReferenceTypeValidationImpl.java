package com.sap.ic.cmh.configuration.validations;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;
import com.sap.ic.cmh.customercomplaint.referencedocumentcategory.persistency.ReferenceDocumentCategoriesDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.configurationservice.ReferenceTypes;
import cds.gen.configurationservice.ReferenceTypes_;

@Component
public class ReferenceTypeValidationImpl implements ReferenceTypeValidation {
	
	private static final Logger logger = LoggerFactory.getLogger(ReferenceTypeValidationImpl.class);
	private static final String REFERENCE_TYPE_VALIDATION_IMPL = "ReferenceTypeValidationImpl";
	
	@Autowired
	Messages messages;
	
	@Autowired
	DataValidator dataValidator;
	
	@Autowired
	ReferenceTypeDao referenceTypeDao;
	
	@Autowired
	ReferenceDocumentCategoriesDao referenceDocumentCategoriesDao;

	/**
	 * Validate reference type
	 * 
	 * @param {@link ReferenceTypes} referenceType
	 * 
	 * @public
	 */
	@Override
	public void validateReferenceType(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateReferenceType");
		validateCode(referenceType);
		validateDescription(referenceType);
		validateReferenceDocumentCategory(referenceType);	
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateReferenceType");
	}
	
	/**
	 * Validate reference type code
	 * 
	 * @param {@link ReferenceTypes} referenceType
	 * 
	 * @public
	 */
	public void validateCode(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateCode");
		if(StringUtils.isBlank(referenceType.getCode())) {
			messages.error(MessageKeys.REFERENCE_TYPE_CODE_IS_MANDATORY).target("in", ReferenceTypes_.class,ReferenceTypes_::code);
		}else{
			dataValidator.validateData(referenceType.getCode(), MessageKeys.INVALID_REFERENCE_TYPE_CODE,ReferenceTypes_.class, ReferenceTypes_::code, true, true);
			Result resultReferenceType=referenceTypeDao.getReferenceTypeBasedOnCode(referenceType.getCode());
			if(!resultReferenceType.list().isEmpty() && !resultReferenceType.first().get().get("ID").toString().equals(referenceType.getId())){
				messages.error(MessageKeys.REFERENCE_TYPE_CODE_EXISTS).target("in", ReferenceTypes_.class,ReferenceTypes_::code);
			}
		}
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateCode");
	}
	
    /**
	 * Validate reference type description
	 * 
	 * @param {@link ReferenceTypes} referenceType
	 * 
	 * @public
	 */
	public void validateDescription(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateDescription");
			dataValidator.validateData(referenceType.getDescription(), MessageKeys.INVALID_REFERENCE_TYPE_DESCRIPTION,ReferenceTypes_.class, ReferenceTypes_::description, false, true);
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateDescription");
	}
	
    /**
	 * Validate reference document category
	 * 
	 * @param {@link ReferenceTypes} referenceType
	 * 
	 * @public
	 */
	public void validateReferenceDocumentCategory(ReferenceTypes referenceType) {
		LoggerHelper.logMethodEntry(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateReferenceDocumentCategory");
		if(StringUtils.isBlank(referenceType.getReferenceDocumentCategoryCode())) {
			messages.error(MessageKeys.REFERENCE_DOCUMENT_CATEGORY_IS_MANDATORY).target("in", ReferenceTypes_.class,ReferenceTypes_::referenceDocumentCategory_code);
		}else {
			Result result=referenceDocumentCategoriesDao.getReferenceDocumentCategoriesBasedOnCode(referenceType.getReferenceDocumentCategoryCode());
			if(!result.first().isPresent()) {
				messages.error(MessageKeys.INVALID_REFERENCE_DOCUMENT_CATEGORY).target("in", ReferenceTypes_.class,ReferenceTypes_::referenceDocumentCategory_code);
			}
		}
		LoggerHelper.logMethodExit(logger, REFERENCE_TYPE_VALIDATION_IMPL, "validateReferenceDocumentCategory");
	}

}
