package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.DestinationConfigurations;

public interface DestinationConfigurationValidation {
    public void validateDestinationConfiguration(DestinationConfigurations destinationConfigurations);
}
