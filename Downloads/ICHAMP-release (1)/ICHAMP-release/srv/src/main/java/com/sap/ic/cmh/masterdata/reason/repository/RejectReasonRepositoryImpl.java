package com.sap.ic.cmh.masterdata.reason.repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.masterdataservice.Reasons;
import cds.gen.masterdataservice.Reasons_;

@Component
public class RejectReasonRepositoryImpl implements RejectReasonRepository{
    
    @Autowired
    PersistenceService db;

    @Override
    public Result fetchReasonBasedOnCode(String reasonCode) {
        return db.run(Select.from(Reasons_.class).columns(Reasons.CODE).where(r->r.code().eq(reasonCode)));
    }

    
    
}
