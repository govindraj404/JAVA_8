package com.sap.ic.cmh.configuration.service;

import com.sap.cds.Result;
import cds.gen.configurationservice.TargetTypes;

public interface TargetTypeConfigurationService {
	public Result getTargetTypeConfigurations();
	public Result getTargetTypeConfigurationCodeBasedOnId(String targetTypeConfigurationId);
	public Result getTargetTypeConfigurationIdBasedOnCode(String targetTypeConfigurationId);
	public TargetTypes getTargetTypesDetails(String id);
	public boolean getActive(String id );
}
