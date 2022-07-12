package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintReasons;
import com.sap.cds.Result;

public interface ComplaintReasonsService {

    public Result getAllComplaintReasonsOrderByIdentifier();
    public ComplaintReasons getComplaintReasonBasedOnID(String reasonId);
    public boolean getActive(String id );
}