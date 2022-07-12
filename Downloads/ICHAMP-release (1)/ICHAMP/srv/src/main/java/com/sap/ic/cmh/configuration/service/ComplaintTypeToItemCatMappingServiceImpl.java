package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ItemCategories;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToItemCatMappingDao;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;

@Service
public class ComplaintTypeToItemCatMappingServiceImpl implements ComplaintTypeToItemCatMappingService {

	private static final Logger logger = LoggerFactory.getLogger(ComplaintTypeToItemCatMappingServiceImpl.class);
	private static final String COMPLAINT_TYPE_ITEM_CATEGORY_SERVICE_IMPL = "ComplaintTypeToItemCatMappingServiceImpl";

	@Autowired
	ComplaintTypeToItemCatMappingDao complaintTypeToItemCatMappingDao;

	@Override
	public Result getComplaintTypeToItemCatMapping() {
		return complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatMappingIdentifier();
	}

	@Autowired
	ItemCategoriesDao itemCategoriesDao;

	@Autowired
	ComplaintTypeConfigurationDao complaintTypeConfigurationDao;
	/**
	 * Get ComplaintType Itemcategory mapping details based on ID
	 */
	@Override
	public ComplaintTypeToItemCategoryMappings getComplaintTypeToItemCatMappingsDetails(String id){
		Result complaintTypeToItemCatMappingsResult = complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatBasedOnId(id);
		return complaintTypeToItemCatMappingsResult.first().isPresent() ? complaintTypeToItemCatMappingsResult.listOf(ComplaintTypeToItemCategoryMappings.class).get(0)
				: null;
	}

	/**
	 * fetch Complaint Type is active flag  based on Id
	 */
	@Override
	public boolean getIsComplaintTypeActive(String id ){
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CATEGORY_SERVICE_IMPL, "getIsComplaintTypeActive");
		Result complaintTypeResult = complaintTypeConfigurationDao.getComplaintTypeDetailsBasedOnId(id);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CATEGORY_SERVICE_IMPL, "getIsComplaintTypeActive");
		if(complaintTypeResult.first().isPresent()){
			return  complaintTypeResult.listOf(ComplaintTypeConfigurations.class).get(0).getIsActive();
		}
		return  false;
	}

	/**
	 * fetch Item Category is active flag  based on Id
	 */
	@Override
	public boolean getIsItemCategoryActive(String id){
		LoggerHelper.logMethodEntry(logger, COMPLAINT_TYPE_ITEM_CATEGORY_SERVICE_IMPL, "getIsItemCategoryActive");
		Result itemCatResult = itemCategoriesDao.getItemCategoryDetailsBasedOnId(id);
		LoggerHelper.logMethodExit(logger, COMPLAINT_TYPE_ITEM_CATEGORY_SERVICE_IMPL, "getIsItemCategoryActive");
		if(itemCatResult.first().isPresent()){
			return  itemCatResult.listOf(ItemCategories.class).get(0).getIsActive();
		}
		return  false;
	}

}
