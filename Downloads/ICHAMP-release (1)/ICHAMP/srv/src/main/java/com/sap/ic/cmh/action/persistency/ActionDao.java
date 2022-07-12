package com.sap.ic.cmh.action.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.masterdataservice.ActionPreconditions_;
import cds.gen.masterdataservice.ActionPreconditions;
import cds.gen.masterdataservice.Actions_;
import cds.gen.masterdataservice.Actions;

@Repository
public class ActionDao {

    @Autowired
    PersistenceService db;
    /**
     * Get the action code for a BO type and 
     * fetch the relevant action pre-condition data
     * @param businessObjectType
     * @return
     */
    public Result getActions(String businessObjectType){
      CqnSelect select = Select.from(Actions_.class).columns(Actions.CODE)
      .where(a->a.businessObjectType_code().eq(businessObjectType));
       Result result = db.run(select);
       String actionCode = result.first().isPresent()?result.list().get(0).get(Actions.CODE).toString() : "";
       //based on BOtype, get code from Action (CRTQN,CRTWC etc)
       //based on this action, get the bostatus code
       return StringUtils.isNotBlank(actionCode) ? getActionPreconditions(actionCode) : null;
    }
    
    /**
     * Fetch action pre-condition data based on the action code
     * @param actionCode
     * @return
     */
    public Result getActionPreconditions(String actionCode){
      CqnSelect select = Select.from(ActionPreconditions_.class).columns(ActionPreconditions.BUSINESS_OBJECT_STATUS_CODE)
      .where(ac->ac.code_code().eq(actionCode));
        return db.run(select);
    }


    
}
