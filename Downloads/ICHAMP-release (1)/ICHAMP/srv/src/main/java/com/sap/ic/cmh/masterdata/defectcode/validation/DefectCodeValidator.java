package com.sap.ic.cmh.masterdata.defectcode.validation;

import cds.gen.masterdataservice.DefectCodes;

public interface DefectCodeValidator {

    void checkInputsSanitized(DefectCodes defectCodes);

}
