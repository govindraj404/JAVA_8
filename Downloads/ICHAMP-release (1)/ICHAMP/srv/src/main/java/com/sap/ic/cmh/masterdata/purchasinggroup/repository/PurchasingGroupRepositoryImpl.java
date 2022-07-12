package com.sap.ic.cmh.masterdata.purchasinggroup.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.PurchasingGroups_;

@Component
public class PurchasingGroupRepositoryImpl implements PurchasingGroupRepository {

    @Autowired
    private PersistenceService db;

    @Override
    public Result fetchPurchasingGroupDetails(String purchasingGroupCode) {
        return db.run(Select.from(PurchasingGroups_.class).where(b -> b.code().eq(purchasingGroupCode)));
    }
    
}
