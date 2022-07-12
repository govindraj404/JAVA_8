package com.sap.ic.cmh.customercomplaint.referencedocumentcategory.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.com.sap.ic.cmh.referencedocumentcategory.ReferenceDocumentCategories_;

@Repository
public class ReferenceDocumentCategoriesDao {
    
	@Autowired
	PersistenceService db;
	
	/**
	 * Get reference document category based on code
	 * 
	 * @public
	 */
	public Result getReferenceDocumentCategoriesBasedOnCode(String code) {
		return (db.run(Select.from(ReferenceDocumentCategories_.class).columns(b->b.code()).where(c->c.code().eq(code))));
	}
}
