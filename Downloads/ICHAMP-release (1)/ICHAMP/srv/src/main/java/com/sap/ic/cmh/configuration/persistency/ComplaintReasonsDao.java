package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintReasons_;
import cds.gen.configurationservice.ComplaintReasons;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class ComplaintReasonsDao {
    @Autowired
    PersistenceService db;

    private static final String COMPLAINT_REASON_DAO = "ComplaintReasonsDao";
    private static final Logger logger = LoggerFactory.getLogger(ComplaintReasonsDao.class);

    /**
     * Fetch all complaint reasons order by identifier
     *
     */
    public Result getAllComplaintReasonsOrderByIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_DAO, "getAllComplaintReasonsOrderByIdentifier");
        CqnSelect reasonsSelect = Select.from(ComplaintReasons_.class).orderBy(c -> c.get("identifier").desc());
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_DAO, "getAllComplaintReasonsOrderByIdentifier");
        return db.run(reasonsSelect);
    }

    /**
     * Fetch all complaint reasons code and ID based on code
     *
     */
    public Result getComplaintReasonCodeAndIdByCode(String code) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_DAO, "getComplaintReasonCodeAndIdByCode");
        CqnSelect reasonsSelect =
                Select.from(ComplaintReasons_.class).columns(ComplaintReasons.CODE, ComplaintReasons.ID)
                        .where(a -> a.code().eq(code));
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_DAO, "getComplaintReasonCodeAndIdByCode");
        return db.run(reasonsSelect);
    }

    /**
     * To get Complaint Reason Based on ID
     */
    public Result getComplaintReasonBasedOnID(String reasonId){
        return db.run(Select.from(ComplaintReasons_.class)
                .columns(b->b.ID()).where(b->b.ID().eq(reasonId)));
    }
    /**
     * To get Complaint Reason Based on ID
     */
    public Result getComplaintReasonDetailsBasedOnID(String reasonId){
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_DAO, "getComplaintReasonDetailsBasedOnID");
        LoggerHelper.logMethodExit(logger, COMPLAINT_REASON_DAO, "getComplaintReasonDetailsBasedOnID");
        return db.run(Select.from(ComplaintReasons_.class)
                .where(b->b.ID().eq(reasonId)));

    }
}