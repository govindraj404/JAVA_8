package com.sap.ic.cmh.masterdata.reason.validation;

import cds.gen.masterdataservice.Reasons;

public interface RejectReasonValidator {
    
    void checkInputsSanitized(Reasons reasons);
}
