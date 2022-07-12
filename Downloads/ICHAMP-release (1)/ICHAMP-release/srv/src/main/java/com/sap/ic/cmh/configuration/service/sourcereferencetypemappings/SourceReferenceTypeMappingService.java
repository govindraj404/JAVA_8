package com.sap.ic.cmh.configuration.service.sourcereferencetypemappings;

import com.sap.cds.Result;
import cds.gen.configurationservice.SourceReferenceTypeMappings;

public interface SourceReferenceTypeMappingService {

    public Result getSourceReferenceTypeMappings();
    public Result getSourceReferenceTypeMappingBasedOnValues(SourceReferenceTypeMappings sourceReferenceTypeMapping);
    public SourceReferenceTypeMappings getSourceReferenceTypeMappingsDetails(String id);
    
}
