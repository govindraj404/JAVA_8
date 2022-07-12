package com.sap.ic.cmh.configuration.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ReferenceTypes_;

@Repository
public class ReferenceTypeDao {
	
	@Autowired
	PersistenceService db;
	
    /**
	 * get reference type based on Id
	 * 
	 * @param {@link String} referenceId
	 * 
	 * @public
	 */
	public Result getReferenceTypeBasedOnId(String referenceId) {
		return db.run(Select.from(ReferenceTypes_.class).columns(b->b.ID()).where(b->b.ID().eq(referenceId)));
	}
	
	/**
	 * get reference type details based on Id
	 * @param {@link String} referenceId
	 * @public
	 */
	public Result getReferenceTypeDetailsBasedOnId(String referenceId) {
		return db.run(Select.from(ReferenceTypes_.class).where(b->b.ID().eq(referenceId)));
	}


   /**
	 * get reference type details
	 * 
	 * @public
	 */
	public Result getReferenceTypeDetails() {
		return db.run(Select.from(ReferenceTypes_.class).columns(b->b.identifier()).orderBy(c -> c.get("identifier").desc()));
	}
	
   /**
	 * get reference type based on code
	 *
	 * @public
	 */
	public Result getReferenceTypeBasedOnCode(String code) {
		return db.run(Select.from(ReferenceTypes_.class).columns(b->b.code(),b->b.ID()).where(b->b.code().eq(code)));
	}
	
}
