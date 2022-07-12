package com.sap.ic.cmh.configuration.validations;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.customercomplaint.complaintquantityrules.persistency.ComplaintQuantityRulesDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.configurationservice.ItemCategories;
import cds.gen.configurationservice.ItemCategories_;

@Component
public class ItemCategoryValidationImpl implements ItemCategoryValidation {

	@Autowired
	DataValidator dataValidator;

	@Autowired
	Messages messages;

	@Autowired
	ItemCategoriesDao itemCategoryDao;

	@Autowired
	ComplaintsDao complaintsDao;

	@Autowired
	ComplaintQuantityRulesDao complaintQuantityRulesDao;

	@Autowired
	MaterialMasterGeneralDataRepository materialMasterGeneralDataRepository;

	private static final Logger logger = LoggerFactory.getLogger(ItemCategoryValidationImpl.class);
	private static final String ITEM_CATEGORY_VALIDATION_IMPL = "ItemCategoryValidationImpl";

	/**
	 * validate item categories
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	@Override
	public void validateItemCategories(ItemCategories itemCategory) {
		validateComplaintCategory(itemCategory);
		validateCode(itemCategory);
		validateDescription(itemCategory);
		validateComplaintQuantityRule(itemCategory);
		validateMaterial(itemCategory);
	}

	/**
	 * validate code
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateCode(ItemCategories itemCategory) {
		LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateCode");
		if (StringUtils.isBlank(itemCategory.getCode())) {
			messages.error(MessageKeys.ITEM_CATEGORY_CODE_IS_MANDATORY).target("in", ItemCategories_.class,
					ItemCategories_::code);
		} else {
			dataValidator.validateData(itemCategory.getCode(), MessageKeys.INVALID_ITEM_CATEGORY_CODE,
					ItemCategories_.class, ItemCategories_::code, true, true);
			Result resultItemCategory = itemCategoryDao.getItemCategoryBasedOnCode(itemCategory.getCode());
			if (!resultItemCategory.list().isEmpty()
					&& !resultItemCategory.first().get().get("ID").toString().equals(itemCategory.getId())) {
				messages.error(MessageKeys.ITEM_CATEGORY_CODE_EXISTS).target("in", ItemCategories_.class,
						ItemCategories_::code);
			}
		}
		LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateCode");
	}

	/**
	 * validate description
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateDescription(ItemCategories itemCategory) {
		LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateDescription");
		if (StringUtils.isNotBlank(itemCategory.getDescription())) {
			dataValidator.validateData(itemCategory.getDescription(), MessageKeys.INVALID_ITEM_CATEGORY_DESCRIPTION,
					ItemCategories_.class, ItemCategories_::description, true, true);
		}
		LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateDescription");
	}

	/**
	 * validate material
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateMaterial(ItemCategories itemCategory) {
		LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateMaterial");
		if (itemCategory.getMaterialId() != null) {
			Result result = materialMasterGeneralDataRepository
					.getMaterialMasterGeneralDataBasedOnId(itemCategory.getMaterialId());
			if (!result.first().isPresent()) {
				messages.error(MessageKeys.INVALID_MATERIAL).target("in", ItemCategories_.class,
						ItemCategories_::material_ID);
			}
		}
		LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateMaterial");
	}

	/**
	 * validate complaint category
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateComplaintCategory(ItemCategories itemCategory) {
		LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateComplaintCategory");
		if (StringUtils.isBlank(itemCategory.getComplaintCategoryCode())) {
			messages.error(MessageKeys.COMPLAINT_CATEGORY_IS_MANDATORY).target("in", ItemCategories_.class,
					ItemCategories_::complaintCategory_code);
		} else {
			Result result = complaintsDao.getComplaintCategoryBasedOnCode(itemCategory.getComplaintCategoryCode());
			if (!result.first().isPresent()) {
				messages.error(MessageKeys.INVALID_COMPLAINT_CATEGORY).target("in", ItemCategories_.class,
						ItemCategories_::complaintCategory_code);
			}
		}
		LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateComplaintCategory");
	}

	/**
	 * validate complaint quantity rule
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateComplaintQuantityRule(ItemCategories itemCategory) {
		LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateComplaintQuantityRule");
		if (StringUtils.isBlank(itemCategory.getComplaintQuantityRuleCode())) {
			messages.error(MessageKeys.COMPLAINT_QUANTITY_RULE_IS_MANDATORY).target("in", ItemCategories_.class,
					ItemCategories_::complaintQuantityRule_code);
		} else {
			Result result = complaintQuantityRulesDao
					.getComplaintQuantityRulesBasedOnCode(itemCategory.getComplaintQuantityRuleCode());
			if (!result.first().isPresent()) {
				messages.error(MessageKeys.INVALID_COMPLAINT_QUANTITY_RULE).target("in", ItemCategories_.class,
						ItemCategories_::complaintCategory_code);
			}
		}
		LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_VALIDATION_IMPL, "validateComplaintQuantityRule");
	}
}
