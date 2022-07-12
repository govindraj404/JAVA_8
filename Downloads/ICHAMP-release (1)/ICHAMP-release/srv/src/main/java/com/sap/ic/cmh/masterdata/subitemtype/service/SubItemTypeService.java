package com.sap.ic.cmh.masterdata.subitemtype.service;

import cds.gen.masterdataservice.SubItemTypes;

public interface SubItemTypeService {

    public SubItemTypes fetchSubItemTypes(String subItemTypeCode);
    
}
