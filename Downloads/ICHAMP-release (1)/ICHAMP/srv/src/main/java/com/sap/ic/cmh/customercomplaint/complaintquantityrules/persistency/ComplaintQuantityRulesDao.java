package com.sap.ic.cmh.customercomplaint.complaintquantityrules.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ComplaintQuantityRules_;

@Repository
public class ComplaintQuantityRulesDao {
    
	@Autowired
	PersistenceService db;
	
	/**
	 * Get complaint quantity rules based on code
	 * 
	 * @public
	 */
	public Result getComplaintQuantityRulesBasedOnCode(String code) {
		return (db.run(Select.from(ComplaintQuantityRules_.class).columns(b->b.code()).where(c->c.code().eq(code))));
	}
}
