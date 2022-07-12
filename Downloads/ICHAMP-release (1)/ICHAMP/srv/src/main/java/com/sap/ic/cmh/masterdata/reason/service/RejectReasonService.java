package com.sap.ic.cmh.masterdata.reason.service;

import cds.gen.masterdataservice.Reasons;

public interface RejectReasonService {

    public Reasons fetchReasonBasedOnCode(String reasonCode);
    
}
