package com.sap.ic.cmh.masterdata.purchasinggroup.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.purchasinggroup.repository.PurchasingGroupRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.masterdataservice.PurchasingGroups;

@Service
public class PurchasingGroupServiceImpl implements PurchasingGroupService {

    @Autowired
    PurchasingGroupRepository purchasingGroupRepository;

    @Override
    public PurchasingGroups fetchPurchasingGroupDetails(String purchasingGroupCode) {
        Result purchasingGroupResult =
                purchasingGroupRepository.fetchPurchasingGroupDetails(purchasingGroupCode);
        return purchasingGroupResult.first().isPresent()
                ? purchasingGroupResult.listOf(PurchasingGroups.class).get(0)
                : null;
    }

}
