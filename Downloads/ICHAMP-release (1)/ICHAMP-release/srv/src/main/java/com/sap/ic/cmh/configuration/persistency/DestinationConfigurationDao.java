package com.sap.ic.cmh.configuration.persistency;

import java.util.List;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.DestinationConfigurations_;

@Repository
public class DestinationConfigurationDao {
    
    @Autowired
    PersistenceService db;

    /**To perform select query on DestinationConfigurations
	*
	*@param {@link DestinationConfigurations} destinationConfigurations
	*
	* @public
	*/
    public Result getDestinationConfiguration(){
        return db
                .run(Select.from(DestinationConfigurations_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**To perform select query on DestinationConfigurations based on destination, company code and business Object
	*
	*@param {@link DestinationConfigurations} destinationConfigurations
	*
	* @public
	*/
    public Result getDestinationConfigBasedOnCompanyAndDestination(String companyCodeID, String boType){
        return db
                .run(Select.from(DestinationConfigurations_.class).where(b -> b.companyCode_ID().eq(companyCodeID).and(b.businessObjectType_code().eq(boType))));
    }

    /**
     * To perform select query on DestinationConfigurations based on company code
     * and business Object
     *
     * @param {@link DestinationConfigurations} destinationConfigurations
     *
     * @public
     */
    public Result getDestinationConfigBasedOnCompanyAndBOType(String companyCodeID, String boType) {
        return db.run(Select.from(DestinationConfigurations_.class).columns(DestinationConfigurations.DESTINATION,DestinationConfigurations.NAVIGATION_DESTINATION)
                .where(b -> b.companyCode_ID().eq(companyCodeID).and(b.businessObjectType_code().eq(boType))));
    }

    /**
     * To perform select query on DestinationConfigurations based on company code
     * @param {@link String} boType
     *
     * @public
     */
    public List<DestinationConfigurations> getDestinationConfigBasedOnBOType(String boType) {
        return db.run(Select.from(DestinationConfigurations_.class)
                .where(b -> b.businessObjectType_code().eq(boType))).listOf(DestinationConfigurations.class);
    }

    public Result getDestinationConfigDetail(String destinationConfigurationID){
        return db.run(Select.from(DestinationConfigurations_.class).where(b -> b.get(DestinationConfigurations.ID).eq(destinationConfigurationID)));
    }
}
