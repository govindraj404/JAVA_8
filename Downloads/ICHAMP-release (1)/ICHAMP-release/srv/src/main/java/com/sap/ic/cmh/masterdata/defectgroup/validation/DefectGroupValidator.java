package com.sap.ic.cmh.masterdata.defectgroup.validation;

import cds.gen.masterdataservice.DefectGroups;

public interface DefectGroupValidator {

    void checkInputsSanitized(DefectGroups defectGroups);

    void checkIfDefectCodeExistForDefectGroup(String sDefectGroup);

}
