package com.sap.ic.cmh.configuration.service.sourcereferencetypemappings;

import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.SourceReferenceTypeMappingDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.sap.ic.cmh.utils.LoggerHelper;

@Service
public class SourceReferenceTypeMappingServiceImpl implements SourceReferenceTypeMappingService {

    @Autowired
    SourceReferenceTypeMappingDao sourceReferenceTypeMappingDao;

    private static final Logger logger = LoggerFactory.getLogger(SourceReferenceTypeMappingServiceImpl.class);
    private static final String SOURCE_REFERENCE_TYPE_MAPPING_SERVICE_IMPL = "SourceReferenceTypeMappingServiceImpl";

    public Result getSourceReferenceTypeMappings(){
        return sourceReferenceTypeMappingDao.getSourceReferenceTypeMappings();
    }
    public Result getSourceReferenceTypeMappingBasedOnValues(SourceReferenceTypeMappings sourceReferenceTypeMapping){
        return sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnValues(sourceReferenceTypeMapping);
    }
    /**
     *
     * fetch SourceReferenceTypeMapping details based on ID.
     *
     * @public
     */
    @Override
    public SourceReferenceTypeMappings getSourceReferenceTypeMappingsDetails(String id){
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_SERVICE_IMPL, "getSourceReferenceTypeMappingsDetails");
        Result sourceReferenceTypeMappingsResult = sourceReferenceTypeMappingDao.getSourceReferenceTypeMappingBasedOnId(id);
        return sourceReferenceTypeMappingsResult.first().isPresent() ? sourceReferenceTypeMappingsResult.
                listOf(SourceReferenceTypeMappings.class).get(0)
                : null;
    }

}
