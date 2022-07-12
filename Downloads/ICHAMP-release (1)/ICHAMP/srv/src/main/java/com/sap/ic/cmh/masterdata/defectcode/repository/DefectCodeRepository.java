package com.sap.ic.cmh.masterdata.defectcode.repository;

import com.sap.cds.Result;

public interface DefectCodeRepository {

    public Result fetchDefectCode(String defectCode, String defectGroup);

    public Result fetchDefectCodeByDefectGroup(String defectGroup);

}
