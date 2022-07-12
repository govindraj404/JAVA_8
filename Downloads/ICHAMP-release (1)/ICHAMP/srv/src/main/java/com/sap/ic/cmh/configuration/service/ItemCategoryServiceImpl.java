package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ItemCategoryServiceImpl implements ItemCategoryService {

    public static final Logger logger = LoggerHelper.getLogger(ItemCategoryServiceImpl.class);
    private static final String ITEM_CATEGORY_SERVICE_IMPL = "ItemCategoryServiceImpl";
    @Autowired
    ItemCategoriesDao itemCategoriesDao;

    /**
     * get item categories
     *
     * @public
     */
    @Override
    public Result getItemCategories() {
        return itemCategoriesDao.getItemCategoryDetails();
    }

    /**
     * fetch item category details based on Id
     */
    @Override
    public ItemCategories getItemCategoryDetails(String id) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_SERVICE_IMPL, "getItemCategoryDetails");
        Result itemCategoriesResult = itemCategoriesDao.getItemCategoryDetailsBasedOnId(id);
        return itemCategoriesResult.first().isPresent() ? itemCategoriesResult.listOf(ItemCategories.class).get(0)
                : null;
    }

    /**
     * fetch item category is active flag  based on Id
     */
    @Override
    public boolean getActive(String id ){
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORY_SERVICE_IMPL, "getActive");
        Result itemCatResult = itemCategoriesDao.getItemCategoryDetailsBasedOnId(id);
        LoggerHelper.logMethodExit(logger, ITEM_CATEGORY_SERVICE_IMPL, "getActive");

        if(itemCatResult.first().isPresent()){
            return  itemCatResult.listOf(ItemCategories.class).get(0).getIsActive();
        }

        return  false;
    }
}
