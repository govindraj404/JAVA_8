package com.sap.ic.cmh.masterdata.unitofmeasure.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.masterdataservice.UnitOfMeasures_;


@Component
public class UnitOfMeasureRepositoryImpl implements UnitOfMeasureRepository {
	
	@Autowired
	PersistenceService db;

	@Override
	public Result getUnitOfMeasureDetails(String unitCode) {
		 return db.run(Select.from(UnitOfMeasures_.class).columns(UnitOfMeasures.CODE,UnitOfMeasures.ISOCODE)
			        .where(b -> b.code().eq(unitCode)));
	}

}
