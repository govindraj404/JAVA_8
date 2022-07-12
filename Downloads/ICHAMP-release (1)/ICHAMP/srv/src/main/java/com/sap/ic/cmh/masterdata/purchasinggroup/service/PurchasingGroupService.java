package com.sap.ic.cmh.masterdata.purchasinggroup.service;

import cds.gen.masterdataservice.PurchasingGroups;

public interface PurchasingGroupService {

    public PurchasingGroups fetchPurchasingGroupDetails(String purchasingGroupCode);
    
}
