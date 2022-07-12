package com.sap.ic.cmh.masterdata.defectcode.service;

import cds.gen.masterdataservice.DefectCodes;

public interface DefectCodeService {

    public DefectCodes fetchDefectCode(String defectCode,String defectGroup);
    
}
