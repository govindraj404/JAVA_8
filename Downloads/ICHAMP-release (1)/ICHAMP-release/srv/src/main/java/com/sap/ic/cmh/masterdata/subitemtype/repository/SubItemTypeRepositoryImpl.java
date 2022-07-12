package com.sap.ic.cmh.masterdata.subitemtype.repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.masterdataservice.SubItemTypes_;

@Component
public class SubItemTypeRepositoryImpl implements SubItemTypeRepository{
    @Autowired
    PersistenceService db;

    @Override
    public Result fetchSubItemTypes(String subItemTypeCode) {
        return db.run(Select.from(SubItemTypes_.class)
        .where(b -> b.code().eq(subItemTypeCode)));
    }
    
}
