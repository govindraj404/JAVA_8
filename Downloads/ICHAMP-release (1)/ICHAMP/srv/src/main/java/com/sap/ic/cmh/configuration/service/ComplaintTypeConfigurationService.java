package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import com.sap.cds.Result;

public interface ComplaintTypeConfigurationService {
    public Result getComplaintTypeConfiguration();

    public ComplaintTypeConfigurations getAllComplaintTypesDetails(String id);

    public boolean getActive(String id);
}
