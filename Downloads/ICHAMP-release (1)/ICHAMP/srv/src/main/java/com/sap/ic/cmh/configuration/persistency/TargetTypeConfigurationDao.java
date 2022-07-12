package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import cds.gen.configurationservice.TargetTypes_;

@Repository
public class TargetTypeConfigurationDao {

    @Autowired
    PersistenceService db;
    private static final Logger logger = LoggerFactory.getLogger(TargetTypeConfigurationDao.class);
    private static final String TARGET_TYPE_CONFIGURATION_DAO = "TargetTypeConfigurationDao";

    /**To perform select query on TargetTypes in descending order of identifier
     *
     *@param {@link TargetTypes} targetTypes
     *
     * @public
     */
    public Result getTargetTypeConfigurations() {
        return db
                .run(Select.from(TargetTypes_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**
     * To perform select query on Target Type configuration code based on id
     */

    public Result getTargetTypeConfigurationCodeBasedOnId(String targetTypeConfigurationId) {
        return db
                .run(Select.from(TargetTypes_.class).columns(b->b.code()).where(b -> b.ID().eq(targetTypeConfigurationId)));
    }

    /**
     * To perform select query on Target Type configuration based on id
     */

    public Result getTargetTypeConfigurationIdBasedOnCode(String targetTypeConfigurationCode) {
        return db
                .run(Select.from(TargetTypes_.class).columns(b->b.ID()).where(b -> b.code().eq(targetTypeConfigurationCode)));
    }
    /**
     * To perform select query for audit log on Target Type configuration code based on id
     *   @public
     */
    public Result getTargetTypeConfigurationCodeBasedOnId1(String id) {
        LoggerHelper.logMethodEntry(logger, TARGET_TYPE_CONFIGURATION_DAO, "getTargetTypeConfigurationCodeBasedOnId1");
        return db.run(Select.from(TargetTypes_.class).where(b->b.ID().eq(id)));
    }

}
