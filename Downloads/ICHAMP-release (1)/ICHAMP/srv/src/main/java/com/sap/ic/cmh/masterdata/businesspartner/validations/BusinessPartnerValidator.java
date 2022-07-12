package com.sap.ic.cmh.masterdata.businesspartner.validations;

import cds.gen.masterdataservice.BusinessPartners;

public interface BusinessPartnerValidator {

    void checkInputsSanitized(BusinessPartners businessPartner);
}
