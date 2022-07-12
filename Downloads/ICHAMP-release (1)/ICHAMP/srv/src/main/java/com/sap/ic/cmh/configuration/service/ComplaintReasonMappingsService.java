package com.sap.ic.cmh.configuration.service;

import com.sap.cds.Result;
import cds.gen.configurationservice.ComplaintReasonMappings;

public interface ComplaintReasonMappingsService {
    public Result getComplaintReasonMapIdentifier();

    public ComplaintReasonMappings getComplaintReasonMappingsDetails(String id);
}
