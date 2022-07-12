package com.sap.ic.cmh.masterdata.defectgroup.repository;

import com.sap.cds.Result;

public interface DefectGroupRepository {

    public Result fetchDefectGroupCode(String defectGroupCode);
    
}
