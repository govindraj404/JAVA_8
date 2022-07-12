package com.sap.ic.cmh.masterdata.purchasinggroup.validation;

import cds.gen.masterdataservice.PurchasingGroups;

public interface PurchasingGroupValidator {

    void checkInputsSanitized(PurchasingGroups purchasingGroups);
}