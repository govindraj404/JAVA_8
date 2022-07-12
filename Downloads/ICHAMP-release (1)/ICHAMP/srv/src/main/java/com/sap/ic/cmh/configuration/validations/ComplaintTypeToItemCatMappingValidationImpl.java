package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.*;
import com.sap.ic.cmh.configuration.service.ComplaintTypeToItemCatMappingService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToItemCatMappingDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;


@Component
public class ComplaintTypeToItemCatMappingValidationImpl implements ComplaintTypeToItemCatMappingValidation {

	@Autowired
 	ConfigurationFieldsValidation fieldsValidation;

	@Autowired
 	MasterDataValidation masterDataValidation;
	@Autowired
	PersistenceService db;
	@Autowired
	Messages messages;
	@Autowired
	ItemCategoriesDao itemCategoriesDao;
	@Autowired
	ComplaintTypeToItemCatMappingDao complaintTypeToItemCatMappingDao;

	@Autowired
	ComplaintTypeToItemCatMappingService complaintTypeToItemCatMappingService;

	private static final Logger logger = LoggerFactory.getLogger(ComplaintTypeToItemCatMappingValidationImpl.class);
    private static final String COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION = "ComplaintTypeToItemCatMappingValidationImpl";
	/**
	 * Input validations for all attributes
	 */
	@Override
	public void validateComplaintTypeToItemCategoryMappings(ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateComplaintTypeToItemCategoryMappings");
		masterDataValidation.validateSalesOrganization(complaintTypeToItemCategoryMappings.getSalesOrganizationId(),ComplaintTypeToItemCategoryMappings_.class, ComplaintTypeToItemCategoryMappings_::salesOrganization_ID);
 		masterDataValidation.validateDistributeChannel(complaintTypeToItemCategoryMappings.getDistributionChannelId(),complaintTypeToItemCategoryMappings.getSalesOrganizationId(),ComplaintTypeToItemCategoryMappings_.class, ComplaintTypeToItemCategoryMappings_::distributionChannel_ID);
 		masterDataValidation.validateDivision(complaintTypeToItemCategoryMappings.getDivisionId(),complaintTypeToItemCategoryMappings.getSalesOrganizationId(),ComplaintTypeToItemCategoryMappings_.class, ComplaintTypeToItemCategoryMappings_::division_ID);
		validateItemCategory(complaintTypeToItemCategoryMappings.getItemCategoryId());
		validateComplaintType(complaintTypeToItemCategoryMappings.getComplaintTypeId());
		validateIfRecordWithUniqueFieldsExists(complaintTypeToItemCategoryMappings);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateComplaintTypeToItemCategoryMappings");
	}

	/**
	 * Check if record exists for the same sales area, complaint type, item category combination
	 * @param complaintTypeToItemCategoryMappings
	 */
    public void validateIfRecordWithUniqueFieldsExists(
			ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
                LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateIfRecordWithUniqueFieldsExists");
		if(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintTypeToItemCategoryMappings)
				.first().isPresent()) {
			messages.error(MessageKeys.UNIQUE_CONSTRAINT_VALIDATION_MESSAGE).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::complaintType_ID);
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateIfRecordWithUniqueFieldsExists");
	}
	/**
     * Validate Complaint Type and its individual complaint type
     * @param complaintTypeId
     */
	public void validateComplaintType(String complaintTypeId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateComplaintType");
		if(StringUtils.isBlank(complaintTypeId)) {
			messages.error(MessageKeys.COMPLAINT_TYPE_IS_MANDATORY).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::complaintType_ID);
		}else {
			Result complaintTypeResult = db.run(Select.from(ComplaintTypeConfigurations_.class).columns(ComplaintTypeConfigurations.ID,ComplaintTypeConfigurations.INDIVIDUAL_COMPLAINT_TYPE).where(b->b.ID().eq(complaintTypeId)));
			Boolean individualComplaintType = false;
			if(complaintTypeResult.first().isPresent()) {
				individualComplaintType = complaintTypeResult.listOf(ComplaintTypeConfigurations.class).get(0).getIndividualComplaintType();
				logger.info("individualComplaintType for the complaint type :: {} ",individualComplaintType);
				if ((individualComplaintType == null || individualComplaintType.equals(false))) {
					messages.error(MessageKeys.COMPLAINT_TYPE_WITH_INDIVIDUAL_COMPLAINT_TYPE).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::complaintType_ID);
				}else if (!complaintTypeToItemCatMappingService.getIsComplaintTypeActive(complaintTypeId)){
					messages.error(MessageKeys.COMPLAINT_ITEMCAT_MAP_COMPLAINT_TYPE_INACTIVE_ERROR).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::complaintType_ID);
				}
			}else {
	 			messages.error(MessageKeys.INVALID_COMPLAINT_TYPE_SPECIFIED).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::complaintType_ID);
			}

		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateComplaintType");
	}
    /**
     * Validate Item Category and its individual complaint type
     * @param itemCategoryId
     */
	public void validateItemCategory(String itemCategoryId) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateItemCategory");
		if(StringUtils.isBlank(itemCategoryId)) {
			messages.error(MessageKeys.COMPLAINT_ITEM_CATEGORY_IS_REQUIRED_FIELD).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::itemCategory_ID);
		}else {
			Result itemCategoryResult = itemCategoriesDao.getComplaintItemCategory(itemCategoryId);
			Boolean individualComplaintType = false;
			if(itemCategoryResult.first().isPresent()) {
				individualComplaintType = itemCategoryResult.listOf(ItemCategories.class).get(0).getIndividualComplaint();
				logger.info("individualComplaintType for the item category :: {} ",individualComplaintType);
				//if item category doesn't belong to individual complaint type
				if ((individualComplaintType == null || individualComplaintType.equals(false))) {
					messages.error(MessageKeys.ITEM_CATEGORY_WITH_INDIVIDUAL_COMPLAINT_TYPE).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::itemCategory_ID);
				}else if (!complaintTypeToItemCatMappingService.getIsItemCategoryActive(itemCategoryId)){
					messages.error(MessageKeys.COMPLAINT_ITEMCAT_MAP_ITEM_CATEGORY_INACTIVE_ERROR).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::itemCategory_ID);
				}
			}else {
	 			messages.error(MessageKeys.INVALID_ITEM_CATEGORY).target("in", ComplaintTypeToItemCategoryMappings_.class,ComplaintTypeToItemCategoryMappings_::itemCategory_ID);
			}
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CAT_MAPPING_VALIDATION, "validateItemCategory");
	}
}
