package com.sap.ic.cmh.configuration.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ComplaintCategories;
import cds.gen.configurationservice.ComplaintCategories_;

@Repository
public class ConfigureItemDao {
	
	@Autowired
	PersistenceService db;
	
	/**
	 * Method to get complaint categories based on code
	 * @param code
	 * @return
	 */
	public Result getComplaintCategoriesBasedOnCode(String code) {
		CqnSelect select = Select.from(ComplaintCategories_.class)
				.where(b -> b.get(ComplaintCategories.CODE).eq(code));
		return db.run(select);
		
	}

}
