package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ReferenceTypes;
import com.sap.cds.Result;

public interface ReferenceTypeService {
	
	public Result getReferenceTypes();

	public ReferenceTypes getReferenceTypesDetails(String id);

}
