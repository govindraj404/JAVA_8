package com.sap.ic.cmh.masterdata.materialmasterplantdata.validation;

import cds.gen.masterdataservice.MaterialMasterPlantDatas;

public interface MaterialMasterPlantDataValidator {
	 void checkInputsSanitized(MaterialMasterPlantDatas plantDat);
}

