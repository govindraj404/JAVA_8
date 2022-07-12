package com.sap.ic.cmh.configuration.service;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.configuration.persistency.ConfigureItemDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ComplaintCategories;
import cds.gen.configurationservice.ConfigureItems;

@Service
public class ConfigureItemServiceImpl implements ConfigureItemService {

	@Autowired
	ConfigureItemDao configureItemDao;
	@Autowired
	LocaleMessageHelper localeMessageHelper;

	private static final Logger logger = LoggerFactory.getLogger(ConfigureItemServiceImpl.class);
	private static final String CONFIG_ITEM_SERVICE = "ConfigureItemService";
	private static final String GET_ALL_CONFIG_APPS = "getAllConfiguredApplications";

	/**
	 * This method is to get all configuration applications
	 * 
	 * @return
	 */
	@Override
	public List<ConfigureItems> getAllConfiguredApplications() {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		List<ConfigureItems> list = new LinkedList<>();

		Map<Map<String, String>, List<ComplaintCategories>> setConfigureItems = setConfigureItems();

		setConfigureItems.forEach((configAppMap, associatedAppList) -> configAppMap.forEach((code, desc) -> {
			ConfigureItems configureItem = Struct.create(ConfigureItems.class);
			configureItem.setCode(code);
			configureItem.setName(desc);
			configureItem.setAssociatedApplications(associatedAppList);
			list.add(configureItem);
		}));
		logger.info("List Size :: {}", list.size());
		logger.info("List :: {}", list);
		LoggerHelper.logMethodExit(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		return list;
	}

	/**
	 * This method is to set configuration applications
	 * related to both Supplier and Customer complaints
	 * 
	 * @return
	 */
	public Map<Map<String, String>, List<ComplaintCategories>> setConfigureItems() {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		ComplaintCategories supplierRecoveryType = getComplaintCategoriesBasedOnCode(Constants.COMPLAINT_TYPE);
		String supplierRecoveryTypeName = null != supplierRecoveryType ? supplierRecoveryType.getName() : "";
		String supplierRecoveryTypeCode = null != supplierRecoveryType ? supplierRecoveryType.getCode() : "";
		ComplaintCategories customerComplaintType = getComplaintCategoriesBasedOnCode(
				Constants.CUSTOMER_COMPLAINT_TYPE);
		String customerComplaintTypeName = null != customerComplaintType ? customerComplaintType.getName() : "";
		String customerComplaintTypeCode = null != customerComplaintType ? customerComplaintType.getCode() : "";

		Map<Map<String, String>, List<ComplaintCategories>> nestedMap = new LinkedHashMap<>();
		setCustomerComplaintConfigurations(customerComplaintTypeName, nestedMap,
				customerComplaintTypeCode);
		setSupplierRecoveryConfigurations(supplierRecoveryTypeName, nestedMap, supplierRecoveryTypeCode);
		LoggerHelper.logMethodExit(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		return nestedMap;

	}

	/**
	 * This method is to set configuration applications
	 * related to Customer complaints
	 * 
	 * @param customerComplaintTypeName
	 * @param supplierRecoveryTypeName
	 * @param nestedMap
	 * @param customerComplaintTypeCode
	 * @param supplierRecoveryTypeCode
	 */
	public void setCustomerComplaintConfigurations(String customerComplaintTypeName,
			Map<Map<String, String>, List<ComplaintCategories>> nestedMap, String customerComplaintTypeCode) {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		ComplaintCategories customerComplaintCategory = Struct.create(ComplaintCategories.class);
		customerComplaintCategory.setCode(customerComplaintTypeCode);
		customerComplaintCategory.setName(customerComplaintTypeName);
		List<ComplaintCategories> customerComplaintCategoryList = new ArrayList<>();
		customerComplaintCategoryList.add(customerComplaintCategory);

		Map<String, String> map = new LinkedHashMap<>();
		map.put("REFTYPE", localeMessageHelper.getMessage(MessageKeys.REFERENCE_TYPE));
		map.put("TARGETTYPE", localeMessageHelper.getMessage(MessageKeys.TARGET_DOCUMENT_TYPE));
		map.put("COMITMRESN", localeMessageHelper.getMessage(MessageKeys.COMPLAINT_REASON));
		map.put("COMPCHNL", localeMessageHelper.getMessage(MessageKeys.COMPLAINT_CHANNEL));
		map.put("ITMCAT", localeMessageHelper.getMessage(MessageKeys.ITEM_CATEGORY));
		map.put("COMTYPE", localeMessageHelper.getMessage(MessageKeys.COMPLAINT_TYPE));
		map.put("COMTYPEITEMCAT", localeMessageHelper.getMessage(MessageKeys.COMPLAINT_TYPE_ITEM_CATEGORY_MAPPING));
		map.put("COMRSNMAP", localeMessageHelper.getMessage(MessageKeys.COMPLAINT_REASON_MAPPING));
		map.put("SOURCEREF", localeMessageHelper.getMessage(MessageKeys.SOURCE_REFERENCE_TYPE_MAPPING));
		map.put("TARGETREF", localeMessageHelper.getMessage(MessageKeys.TARGET_DOCUMENT_TYPE_MAPPING));
		nestedMap.put(map, customerComplaintCategoryList);

		logger.info("customerComplaintCategoryList size :: {} ", customerComplaintCategoryList.size());
		LoggerHelper.logMethodExit(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
	}

	/**
	 * This method is to set configuration applications
	 * related to Supplier complaints
	 * 
	 * @param supplierRecoveryTypeName
	 * @param nestedMap
	 * @param supplierRecoveryTypeCode
	 */
	public void setSupplierRecoveryConfigurations(String supplierRecoveryTypeName,
			Map<Map<String, String>, List<ComplaintCategories>> nestedMap, String supplierRecoveryTypeCode) {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
		logger.info("Inside setSupplierRecoveryConfigurations");
		ComplaintCategories supplierRecoveryCategory = Struct.create(ComplaintCategories.class);
		supplierRecoveryCategory.setCode(supplierRecoveryTypeCode);
		supplierRecoveryCategory.setName(supplierRecoveryTypeName);
		List<ComplaintCategories> supplierRecoveryCategoryList = new ArrayList<>();
		supplierRecoveryCategoryList.add(supplierRecoveryCategory);

		Map<String, String> map = new LinkedHashMap<>();
		map.put("DESCONFIG", localeMessageHelper.getMessage(MessageKeys.DESTINATION));
		map.put("BUSOBJCONFIG", localeMessageHelper.getMessage(MessageKeys.BUSINESS_OBJECT));
		map.put("CONTYPE", localeMessageHelper.getMessage(MessageKeys.CONDITION_TYPE));
		map.put("SERMAT", localeMessageHelper.getMessage(MessageKeys.SERVICE_MATERIAL));
		map.put("CLMSTSMAP", localeMessageHelper.getMessage(MessageKeys.CLAIM_STATUS));
		nestedMap.put(map, supplierRecoveryCategoryList);

		logger.info("setSupplierRecoveryConfigurations configureItemMap :: {} ", nestedMap);
		LoggerHelper.logMethodExit(logger, CONFIG_ITEM_SERVICE, GET_ALL_CONFIG_APPS);
	}

	/**
	 * Method to get complaint categories based on code
	 * 
	 * @param code
	 * @return
	 */
	public ComplaintCategories getComplaintCategoriesBasedOnCode(String code) {
		Result complaintCategoriesBasedOnCodeResult = configureItemDao.getComplaintCategoriesBasedOnCode(code);
		return complaintCategoriesBasedOnCodeResult.first().isPresent()
				? complaintCategoriesBasedOnCodeResult.listOf(ComplaintCategories.class).get(0)
				: null;
	}

}
