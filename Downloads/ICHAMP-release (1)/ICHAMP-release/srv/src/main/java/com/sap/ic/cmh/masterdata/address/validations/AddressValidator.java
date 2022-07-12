package com.sap.ic.cmh.masterdata.address.validations;

import cds.gen.masterdataservice.Addresses;

public interface AddressValidator {

    void checkInputsSanitized(Addresses address);
}
