package com.sap.ic.cmh.masterdata.companycode.validations;

import cds.gen.masterdataservice.CompanyCodes;

public interface CompanyCodeValidator {

    void checkInputsSanitized(CompanyCodes companyCode);
}
