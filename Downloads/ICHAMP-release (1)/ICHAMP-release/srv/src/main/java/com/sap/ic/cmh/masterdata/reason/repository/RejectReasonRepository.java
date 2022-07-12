package com.sap.ic.cmh.masterdata.reason.repository;

import com.sap.cds.Result;

public interface RejectReasonRepository {

    public Result fetchReasonBasedOnCode(String reasonCode);
    
}
