package com.sap.ic.cmh.configuration.service.targetreferencetypemappings;

import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeMappingsDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.configurationservice.TargetReferenceTypeMappings;

    @Service
    public class TargetReferenceTypeMappingServiceImpl implements TargetReferenceTypeMappingService {

        @Autowired
        TargetReferenceTypeMappingsDao targetReferenceTypeMappingDao;

        /**
         * To get Target Reference Type
         * @public
         */
        public Result getTargetReferenceTypeMappings(){
            return targetReferenceTypeMappingDao.getTargetReferenceTypeMappings();
        }
        /**
         * To get Target Reference Type Based on Values
         * @param {link TargetReferenceTypeMappings} targetReferenceTypeMapping
         * @public
         */
        public Result getTargetReferenceTypeMappingBasedOnValues(TargetReferenceTypeMappings targetReferenceTypeMapping){
            return targetReferenceTypeMappingDao.getTargetReferenceTypeMappingBasedOnValues(targetReferenceTypeMapping);
        }
        /**
         * To get Target Reference Type Based on ID
         * @param {link String} id
         * @public
         */
        @Override
        public TargetReferenceTypeMappings getRefrenceTypeMappingBasedOnID(String id) {
            Result targetReferenceTypeMapping=targetReferenceTypeMappingDao.getTargetReferenceTypeMappingDetailsBasedOnID(id);
            return targetReferenceTypeMapping.first().isPresent() ? targetReferenceTypeMapping.listOf(TargetReferenceTypeMappings.class).get(0)
                    : null;
        }
    }
