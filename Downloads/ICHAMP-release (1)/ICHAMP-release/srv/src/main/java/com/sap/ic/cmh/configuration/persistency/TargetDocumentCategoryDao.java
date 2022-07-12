package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import cds.gen.configurationservice.TargetDocumentCategories_;

@Repository
public class TargetDocumentCategoryDao {

    @Autowired
    PersistenceService db;
    
    /**
    	* To perform select query on Target Type category based on code
    */

    public Result getTargetDocumentCategoryBasedOnCode(String targetDocumentCategoryCode) {
        return db
        .run(Select.from(TargetDocumentCategories_.class).where(b -> b.code().eq(targetDocumentCategoryCode)));
    }
}
