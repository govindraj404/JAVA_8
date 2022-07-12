package com.sap.ic.cmh.configuration.service.targetreferencetypemappings;

import com.sap.cds.Result;
import cds.gen.configurationservice.TargetReferenceTypeMappings;

public interface TargetReferenceTypeMappingService {

    public Result getTargetReferenceTypeMappings();
    public Result getTargetReferenceTypeMappingBasedOnValues(TargetReferenceTypeMappings targetReferenceTypeMapping);

    public TargetReferenceTypeMappings getRefrenceTypeMappingBasedOnID(String id);
}