package com.sap.ic.cmh.masterdata.purchasinggroup.repository;

import com.sap.cds.Result;

public interface PurchasingGroupRepository {

    public Result fetchPurchasingGroupDetails(String purchasingGroupCode);
    
}
