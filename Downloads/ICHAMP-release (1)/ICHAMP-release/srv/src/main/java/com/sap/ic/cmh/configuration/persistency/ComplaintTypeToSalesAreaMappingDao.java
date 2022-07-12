package com.sap.ic.cmh.configuration.persistency;

 import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings_;

 @Repository
 public class ComplaintTypeToSalesAreaMappingDao {

 	@Autowired
     PersistenceService db;

	 /**
	 * To perform select query on DestinationMapping
 	 *
 	 */
     public Result getSalesAreaMappingBasedOnComplaintTypeConfig(String complaintTypeConfigId){
         return db.run(Select.from(ComplaintTypeToSalesAreaMappings_.class)
         		.columns(b->b.salesOrganization_ID(),b->b.distributionChannel_ID(), b->b.division_ID())
         		.where(c->c.complaintTypeConfiguration_ID().eq(complaintTypeConfigId)));
     }

        
 }