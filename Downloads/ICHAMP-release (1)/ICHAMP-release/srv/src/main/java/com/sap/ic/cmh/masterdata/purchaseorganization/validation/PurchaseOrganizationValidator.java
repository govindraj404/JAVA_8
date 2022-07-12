package com.sap.ic.cmh.masterdata.purchaseorganization.validation;

import cds.gen.masterdataservice.PurchaseOrganizations;

public interface PurchaseOrganizationValidator {
    void checkInputsSanitized(PurchaseOrganizations purchaseOrganizations);
}
