 package com.sap.ic.cmh.configuration.validations;

 import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

 import com.sap.ic.cmh.configuration.service.ItemCategoryService;
 import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.com.sap.ic.cmh.complainttype.ComplaintTypes;
import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ComplaintTypeConfigurations_;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings_;
import cds.gen.configurationservice.ItemCategories;

 @Component
 public class ComplaintTypeConfigurationValidationImpl implements ComplaintTypeConfigurationValidation {
	
 	@Autowired
 	Messages messages;

 	@Autowired
 	ComplaintTypeConfigurationDao complaintTypeConfigDao;

 	@Autowired
 	ConfigurationFieldsValidation fieldsValidation;

 	@Autowired
 	DataValidator dataValidator;

 	@Autowired
 	MasterDataValidation masterDataValidation;
 	
	@Autowired
	ComplaintsDao complaintsDao;

	 @Autowired
	 ItemCategoryService service;
 	private static final Logger logger = LoggerFactory.getLogger(ComplaintTypeConfigurationValidationImpl.class);
 	private static final String COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL = "ComplaintTypeConfigurationValidationImpl";
 	
 	/**
 	 * Validate complaint type configuration
 	 * 
 	 * @param {@link ComplaintTypeConfigurations} complaintType
 	 * 
 	 * @public
 	 */
	@Override
	public void validateCompliantTypeConfiguration(ComplaintTypeConfigurations complaintType) {
 		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateCompliantTypeConfiguration");
		validateCode(complaintType);
		validateDescription(complaintType);
		validateComplaintCategory(complaintType);
		validateDefaultItemCategory(complaintType.getItemCategoryId(),complaintType.getIndividualComplaintType());
		validateComplaintTypeToSalesAreaMappings(complaintType.getComplaintTypeToSalesAreaMappings());	
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateCompliantTypeConfiguration");
	}

 	/**
 	 * Validate description and default item category
 	 * 
 	 * @param {@link ComplaintTypeConfigurations} complaintType
 	 * 
 	 * @public
 	 */
 	public void validateDescription(ComplaintTypeConfigurations complaintType) {
 		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateDescription");
 		dataValidator.validateData(complaintType.getDescription(), MessageKeys.INVALID_COMPLAINT_TYPE_CONFIG_DESCRIPTION,ComplaintTypeConfigurations_.class, ComplaintTypeConfigurations_::description, false, true);
 		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateDescription");
 	}

 	/**
 	 * validate code
 	 * 
 	 * @param {@link ComplaintTypes} complaintType
 	 * 
 	 * @public
 	 */
 	public void validateCode(ComplaintTypeConfigurations complaintType) {
 		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateCode");
 		if(StringUtils.isBlank(complaintType.getCode())) {
			messages.error(MessageKeys.COMPLAINT_TYPE_CONFIG_CODE_IS_MANDATORY).target("in", ComplaintTypeConfigurations_.class,ComplaintTypeConfigurations_::code);
		}else{
			dataValidator.validateData(complaintType.getCode(), MessageKeys.INVALID_COMPLAINT_TYPE_CONFIG_CODE,ComplaintTypeConfigurations_.class, ComplaintTypeConfigurations_::code, true, true);
			Result resultComplaintType=complaintTypeConfigDao.getComplaintTypeConfigurationBasedOnCode(complaintType.getCode());
			if(!resultComplaintType.list().isEmpty() && !resultComplaintType.first().get().get("ID").toString().equals(complaintType.getId())){
				messages.error(MessageKeys.COMPLAINT_TYPE_CONFIG_CODE_EXIST).target("in", ComplaintTypeConfigurations_.class,ComplaintTypeConfigurations_::code);
			}
		}
 		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateCode");
 	}
	
 	/**
 	 * Perform validation on sales organization, distribution channel, division and
 	 * item category
 	 * 
 	 * @param {@link List<ComplaintTypeToSalesAreaMappings>} salesAreaMapping
 	 * 
 	 * @public
 	 */
 	public void validateComplaintTypeToSalesAreaMappings(List<ComplaintTypeToSalesAreaMappings> salesAreaMapping) {
 		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateComplaintTypeToSalesAreaMappings");
 		if(Boolean.TRUE.equals(validateUniqueComplaintTypeToSalesAreaMappings(salesAreaMapping))) {
 			for (ComplaintTypeToSalesAreaMappings salesArea : salesAreaMapping) {
 				masterDataValidation.validateSalesOrganization(salesArea.getSalesOrganizationId(),ComplaintTypeToSalesAreaMappings_.class, ComplaintTypeToSalesAreaMappings_::salesOrganization_ID);
 		 		masterDataValidation.validateDistributeChannel(salesArea.getDistributionChannelId(),salesArea.getSalesOrganizationId(),ComplaintTypeToSalesAreaMappings_.class, ComplaintTypeToSalesAreaMappings_::distributionChannel_ID);
 		 		masterDataValidation.validateDivision(salesArea.getDivisionId(),salesArea.getSalesOrganizationId(),ComplaintTypeToSalesAreaMappings_.class, ComplaintTypeToSalesAreaMappings_::division_ID);
 			}
 		}
 		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateComplaintTypeToSalesAreaMappings");
 	}

 	/**
 	 * validate default item category
 	 * 
 	 * @param {@link String,Boolean} itemCategoryId,individualComplaintType
 	 * 
 	 * @public
 	 */
 	public void validateDefaultItemCategory(String itemCategoryId, Boolean individualComplaintType) {
 		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateDefaultItemCategory");
 		if ((individualComplaintType == null || individualComplaintType.equals(false)) && itemCategoryId == null) {
 			messages.error(MessageKeys.DEFAULT_ITEM_CATEGORY_IS_MANDATORY).target("in", ComplaintTypeConfigurations_.class, ComplaintTypeConfigurations_::itemCategory_ID);
 		}
 		else if(null!=itemCategoryId && Boolean.FALSE.equals(fieldsValidation.checkItemCategoryExist(itemCategoryId))) {
 			messages.error(MessageKeys.INVALID_DEFAULT_ITEM_CATEGORY).target("in", ComplaintTypeConfigurations_.class, ComplaintTypeConfigurations_::itemCategory_ID);
 		}
		else if ((individualComplaintType == null || individualComplaintType.equals(false)) && !service.getActive(itemCategoryId)){

			messages.error(MessageKeys.ITEM_CATEGORY_INACTIVE_ERROR).target("in", ComplaintTypeConfigurations_.class,ComplaintTypeConfigurations_::itemCategory_ID);
		}
 		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateDefaultItemCategory");
     }
	
 	/**
 	 * validate unique complaint type to sales are mapping
 	 * 
 	 * @param {@link List<ComplaintTypeToSalesAreaMappings>} salesAreaMapping
 	 *              
 	 * @public
 	 */
 	public Boolean validateUniqueComplaintTypeToSalesAreaMappings(List<ComplaintTypeToSalesAreaMappings> salesAreaMapping) {
 		String[] salesAreaMap = new String[3];
 		HashSet<String> set = new HashSet<>();
 		for (ComplaintTypeToSalesAreaMappings salesArea : salesAreaMapping) {
			salesAreaMap[0] = salesArea.getSalesOrganizationId();
			salesAreaMap[1] = salesArea.getDistributionChannelId();
			salesAreaMap[2] = salesArea.getDivisionId();
 			if (!set.add(Arrays.toString(salesAreaMap))) {
 				messages.error(MessageKeys.DUPLICATE_SALES_AREA_MAPPING).target("in",ComplaintTypeConfigurations_.class, ComplaintTypeConfigurations_::complaintTypeToSalesAreaMappings);
 				return false;
 			}
 		}
 		return true;
 	}
 	
	/**
	 * validate complaint category
	 * 
	 * @param {@link ItemCategories} itemCategory
	 * 
	 * @public
	 */
	public void validateComplaintCategory(ComplaintTypeConfigurations complaintType) {
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateComplaintCategory");
		if(StringUtils.isBlank(complaintType.getComplaintCategoryCode())) {
			messages.error(MessageKeys.COMPLAINT_CATEGORY_IS_MANDATORY).target("in", ComplaintTypeConfigurations_.class,ComplaintTypeConfigurations_::complaintCategory_code);
		}else {
			Result result=complaintsDao.getComplaintCategoryBasedOnCode(complaintType.getComplaintCategoryCode());
			if(!result.first().isPresent()) {
				messages.error(MessageKeys.INVALID_COMPLAINT_CATEGORY).target("in", ComplaintTypeConfigurations_.class,ComplaintTypeConfigurations_::complaintCategory_code);
			}
		}
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_CONFIG_VALIDATION_IMPL, "validateComplaintCategory");
	}

 }
