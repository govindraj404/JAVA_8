package com.sap.ic.cmh.masterdata.unitofmeasure.persistency;

import com.sap.cds.Result;

public interface UnitOfMeasureRepository {
	
	Result getUnitOfMeasureDetails (String unitCode);

}
