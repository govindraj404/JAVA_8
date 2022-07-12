package com.sap.ic.cmh.configuration.service;

import com.sap.cds.Result;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;

public interface ComplaintTypeToItemCatMappingService {

    Result getComplaintTypeToItemCatMapping();

    public ComplaintTypeToItemCategoryMappings getComplaintTypeToItemCatMappingsDetails(String id);

    public boolean getIsComplaintTypeActive(String id);

    public boolean getIsItemCategoryActive(String id);
}
