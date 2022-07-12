package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.BusinessObjectConfigurations_;



@Repository
public class BusinessObjectConfigurationDao {
    
    @Autowired
    PersistenceService db;


    /**To perform select query on BusinessObjectConfiguration
	*
	*@param {@link BusinessObjectConfiguration} businessObjectConfiguration
	*
	* @public
	*/
    public Result getBusinessObjectConfigurations() {
        return db.run(Select.from(BusinessObjectConfigurations_.class).orderBy(c -> c.get("identifier").desc()));
    }

    public Result getBusinessObjectConfigurationsDetails(String businessObjectConfigurationId) {
		
		CqnSelect select = Select.from(BusinessObjectConfigurations_.class).where(b -> b.get(BusinessObjectConfigurations.ID).eq(businessObjectConfigurationId));
		return db.run(select);
	}

    /**To perform select query on BusinessObjectConfigurations based on complaint type, destination and BO
	*
	*@param {@link BusinessObjectConfigurations} businessObjectConfigurations
	*
	* @public
    */
    public Result getBusinessObjectConfigBasedOnDestinationAndBO(String complaintType, String destination, String boType, String boAttr){
        return db
                .run(Select.from(BusinessObjectConfigurations_.class).where(b -> b.complaintType_code()
                .eq(complaintType).and(b.businessObjectType_code().eq(boType)).and(b.destination().eq(destination)).and(b.businessObjectAttribute_code().eq(boAttr))));
    }
    
    /**To perform select query on BusinessObjectConfigurations based on complaint type, destination and BO
   	*
   	*@param {@link BusinessObjectConfigurations} businessObjectConfigurations
   	*
   	* @public
       */
       public Result getBusinessObjectConfigBasedOnDestinationAndBOAndDest(String complaintType, String boType,String destination){
          return db
                   .run(Select.from(BusinessObjectConfigurations_.class)
                   .columns(BusinessObjectConfigurations.BUSINESS_OBJECT_ATTRIBUTE_CODE,BusinessObjectConfigurations.BUSINESS_OBJECT_VALUE)
                   .where(b -> b.complaintType_code()
                   .eq(complaintType).and(b.businessObjectType_code().eq(boType)
                		   .and(b.destination().eq(destination)))));
       }

}
