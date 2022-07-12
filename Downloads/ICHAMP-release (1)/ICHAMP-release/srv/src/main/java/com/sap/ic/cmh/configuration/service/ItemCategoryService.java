package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;

public interface ItemCategoryService {
    public Result getItemCategories();

    public ItemCategories getItemCategoryDetails(String id);
    public boolean getActive(String id );
}
