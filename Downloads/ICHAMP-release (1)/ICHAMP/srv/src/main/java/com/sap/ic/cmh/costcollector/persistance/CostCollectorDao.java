package com.sap.ic.cmh.costcollector.persistance;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.costcollectorservice.CostCollectors_;

@Repository
public class CostCollectorDao {


    @Autowired
    PersistenceService db;

    public static final Logger logger = LoggerHelper.getLogger(CostCollectorDao.class);
  	private static final String COST_COLLECTOR_DAO = "CostCollectorDao";
    /**
     * Insert the costCollector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    public CostCollectors insertCostCollector(CostCollectors costCollector) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "insertCostCollector");
        CqnInsert update = Insert.into(CostCollectors_.class).entry(costCollector);
        Result result = db.run(update);
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "insertCostCollector");
        return result.first().isPresent() ? result.listOf(CostCollectors.class).get(0) : null;
    }


    /**
     * Update the costCollector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    public CostCollectors updateCostCollector(CostCollectors costCollector) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "updateCostCollector");
        CqnUpdate update = Update.entity(CostCollectors_.class).data(costCollector)
                .where(b -> b.ID().eq(costCollector.getId()));
        Result result = db.run(update);
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "updateCostCollector");
        return result.first().isPresent() ? result.listOf(CostCollectors.class).get(0) : null;
    }


    /**
     * Delete the costCollector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    public CostCollectors deleteCostCollector(CostCollectors costCollector) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "deleteCostCollector");
        CqnDelete delete =
                Delete.from(CostCollectors_.class).where(b -> b.ID().eq(costCollector.getId()));
        Result result = db.run(delete);
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "deleteCostCollector");
        return result.first().isPresent() ? result.listOf(CostCollectors.class).get(0) : null;
    }


    /**
     * Slect the costCollector where transfer to claim is enabled
     * 
     * @param complaintId
     * @return List<CostCollectors>
     */
    public Result selectTransferToClaimCostCollector(String complaintId) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "selectTransferToClaimCostCollector");
        CqnSelect cqnSelect =
                Select.from(CostCollectors_.class).where(b -> b.get(CostCollectors.PARENT_ID)
                        .eq(complaintId).and(b.get(CostCollectors.TRANSFER_TO_CLAIM).eq(true)));
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "selectTransferToClaimCostCollector");
        return db.run(cqnSelect);
    }


    /**
     * Select all the costCollector
     * 
     * @param complaintID
     * @return List<CostCollectors>
     */
    public Result selectAllCostCollector(String complaintID) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "selectAllCostCollector");
        CqnSelect cqnSelect = Select.from(CostCollectors_.class)
                .where(b -> b.get(CostCollectors.PARENT_ID).eq(complaintID));
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "selectAllCostCollector");
        return db.run(cqnSelect);
    }

    /**
     * Get the costCollector
     * 
     * @param ID
     * @return CostCollectors
     */
    public Result getCostCollector(String id) {
    	LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_DAO, "GetCostCollector");
        CqnSelect cqnSelect = Select.from(CostCollectors_.class)
                .where(b -> b.get(CostCollectors.ID).eq(id));
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_DAO, "GetCostCollector");
        return db.run(cqnSelect);
    }

}
