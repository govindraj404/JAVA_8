package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.BusinessObjectConfigurations;

public interface BusinessObjectConfigurationValidation {
    public void validateBusinessObjectConfiguration(BusinessObjectConfigurations businessObjectConfigurations);
}
