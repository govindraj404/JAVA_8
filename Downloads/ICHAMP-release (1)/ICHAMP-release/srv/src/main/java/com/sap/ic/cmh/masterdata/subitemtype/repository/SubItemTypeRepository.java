package com.sap.ic.cmh.masterdata.subitemtype.repository;

import com.sap.cds.Result;

public interface SubItemTypeRepository {

    public Result fetchSubItemTypes(String subItemTypeCode);
    
}
