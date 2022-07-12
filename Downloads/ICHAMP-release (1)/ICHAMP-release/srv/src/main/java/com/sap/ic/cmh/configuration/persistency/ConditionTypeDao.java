package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ConditionTypes_;

@Repository
public class ConditionTypeDao {
    
    @Autowired
    PersistenceService db;

    /**To perform select query on ConditionTypes
	*
	*@param {@link ConditionTypes} conditionTypes
	*
	* @public
	*/
    public Result getConditionTypes() {
        return db
                .run(Select.from(ConditionTypes_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**To perform select query on ConditionTypes based on destination and item type
	*
	*@param {@link ConditionTypes} conditionTypes
	*
	* @public
	*/
    public Result getConditionTypesBasedOnDestinationAndItemType(String destination, String itemType){
        return db
        .run(Select.from(ConditionTypes_.class).columns(ConditionTypes.ID,ConditionTypes.CONDITION_TYPE)
        .where(b -> b.destination()
        .eq(destination).and(b.itemType_code().eq(itemType))));
    }

    public Result getConditionTypesDetail(String conditionTypeID){
        return db.run(Select.from(ConditionTypes_.class).where(b -> b.get(ConditionTypes.ID).eq(conditionTypeID)));
    }

}
