package com.sap.ic.cmh.masterdata.materialmastergeneraldata.validation;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;

public interface MaterialMasterGeneralDataValidator {
	void checkInputsSanitized(MaterialMasterGeneralDatas generalData);

}
