package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import cds.gen.configurationservice.TargetReferenceTypes_;

@Repository
public class TargetReferenceTypeDao {
    
    @Autowired
    PersistenceService db;

    /**To perform select query on TargetReferenceTypes
	*
	*@param {@link TargetReferenceTypes} targetReferenceTypes
	*
	* @public
	*/
    public Result getTargetReferenceTypes() {
        return db
                .run(Select.from(TargetReferenceTypes_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**To perform select query on TargetReferenceTypes
	*
	*@param {@link TargetReferenceTypes} targetReferenceTypes
	*
	* @public
	*/
    public Result getTargetReferenceTypesBasedOnTargetMapping(String targetReferenceTypeMappingId) {
        return db
                .run(Select.from(TargetReferenceTypes_.class).where(b -> b.targetReferenceTypeMapping_ID()
                .eq(targetReferenceTypeMappingId)));
    }
}
