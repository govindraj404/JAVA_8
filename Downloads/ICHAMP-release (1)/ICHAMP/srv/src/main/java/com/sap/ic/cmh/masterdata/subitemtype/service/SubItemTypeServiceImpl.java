package com.sap.ic.cmh.masterdata.subitemtype.service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.subitemtype.repository.SubItemTypeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.masterdataservice.SubItemTypes;

@Service
public class SubItemTypeServiceImpl implements SubItemTypeService {

    @Autowired
    SubItemTypeRepository subItemTypeRepository;

    @Override
    public SubItemTypes fetchSubItemTypes(String subItemTypeCode) {
        Result subItemTypeResult = subItemTypeRepository.fetchSubItemTypes(subItemTypeCode);
        return subItemTypeResult.first().isPresent()
                ? subItemTypeResult.listOf(SubItemTypes.class).get(0)
                : null;
    }

}
