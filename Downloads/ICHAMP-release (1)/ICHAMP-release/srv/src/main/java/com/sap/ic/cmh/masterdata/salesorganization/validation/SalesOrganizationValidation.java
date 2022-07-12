package com.sap.ic.cmh.masterdata.salesorganization.validation;

import cds.gen.masterdataservice.SalesOrganizations;

public interface SalesOrganizationValidation {

    void checkInputsSanitized(SalesOrganizations salesOrganization);
}
