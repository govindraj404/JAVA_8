package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ClaimStatusMappings_;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import java.util.Optional;

@Repository
public class ClaimStatusMappingsDao {
    @Autowired
    PersistenceService db;

    /**To perform select query on ConditionTypes
     *
     *@param {@link ConditionTypes} conditionTypes
     *
     * @public
     */
    public Optional<ClaimStatusMappings> getClaimStatusMappings(String code) {
        return db
                .run(Select.from(ClaimStatusMappings_.class).where(b -> b.code().eq(code))).first(ClaimStatusMappings.class);
    }
    
    /**
     * Select from ClaimstatusMapping based on order of identifier
     * @return
     */
    public Result getClaimStatusMappingsBasedOnIdentifier() {
    	return db
                .run(Select.from(ClaimStatusMappings_.class).orderBy(c -> c.get("identifier").desc()));
    	
    }
    public Result getClaimStatusMappingDetails(String claimStatusMappingId){
        return db.run(Select.from(ClaimStatusMappings_.class).where(b -> b.get(ClaimStatusMappings.ID).eq(claimStatusMappingId)));
    }
}
