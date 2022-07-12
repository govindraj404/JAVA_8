package com.sap.ic.cmh.masterdata.division.validation;

import cds.gen.masterdataservice.Divisions;

public interface DivisionValidation {

    void checkInputsSanitized(Divisions division);
}
