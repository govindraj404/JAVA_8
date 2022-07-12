package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ItemCategories;
import cds.gen.configurationservice.ItemCategories_;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class ItemCategoriesDao {

    public static final Logger logger = LoggerHelper.getLogger(ItemCategoriesDao.class);
    private static final String ITEM_CATEGORIES_DAO = "ItemCategoriesDao";

    @Autowired
    PersistenceService db;

    /**
     * To perform select query on complaint item category
     */
    public Result getComplaintItemCategory(String itemCategoryId) {
        return db.run(Select.from(ItemCategories_.class).columns(ItemCategories.ID, ItemCategories.INDIVIDUAL_COMPLAINT).where(b -> b.ID().eq(itemCategoryId)));
    }

    /**
     * get item category based on code
     */
    public Result getItemCategoryBasedOnCode(String code) {
        return db.run(Select.from(ItemCategories_.class).columns(b -> b.ID()).where(b -> b.code().eq(code)));
    }

    /**
     * get item category details for sequence number generation
     */
    public Result getItemCategoryDetails() {
        return db.run(Select.from(ItemCategories_.class).columns(b -> b.identifier()).orderBy(c -> c.get("identifier").desc()));
    }

    /**
     * fetch item category based on ID.
     *
     * @public
     */
    public Result getItemCategoryDetailsBasedOnId(String itemCategoryId) {
        LoggerHelper.logMethodEntry(logger, ITEM_CATEGORIES_DAO, "getItemCategoryDetailsBasedOnId");
        return db.run(Select.from(ItemCategories_.class).where(b -> b.ID().eq(itemCategoryId)));
    }
} 
