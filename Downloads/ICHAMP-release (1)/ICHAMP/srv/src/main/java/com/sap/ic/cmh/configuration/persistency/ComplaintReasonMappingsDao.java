package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintReasonMappings_;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class ComplaintReasonMappingsDao {

    @Autowired
    PersistenceService db;

    private static final String COMPLAINT_REASON_MAPPINGS_DAO = "ComplaintReasonMappingsDao";
    private static final Logger logger = LoggerFactory.getLogger(ComplaintReasonMappingsDao.class);

    /**
     * Get Complaint Reason Map order by identifier
     */
    public Result getComplaintReasonMapOrderByIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_DAO, "getComplaintReasonMapOrderByIdentifier");

        return db.run(Select.from(ComplaintReasonMappings_.class)
                .columns(b -> b.identifier()).orderBy(c -> c.get("identifier").desc()));
    }

    /**
     * Get Complaint Reason Map based on attributeIDs
     *
     */
    public Result getComplaintReasonMapBasedOnAttributeIDs(String salesOrgID, String distChannelID,
                                                           String divisionID, String complaintID, String reasonID) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_REASON_MAPPINGS_DAO, "getComplaintReasonMapBasedOnAttributeIDs");
        return db.run(Select.from(ComplaintReasonMappings_.class)
                .columns(b->b.ID()).where(o->o.salesOrganization_ID().eq(salesOrgID)
                        .and(o.distributionChannel_ID().eq(distChannelID))
                        .and(o.division_ID().eq(divisionID))
                        .and(o.itemCategory_ID().eq(complaintID))
                        .and(o.complaintReason_ID().eq(reasonID))));
    }
    /**
     * get complaint reasonmappings type details based on Id
     * @param {@link String} complaintReasonId
     * @public
     */
    public Result getComplaintReasonMapDetailsBasedOnId(String complaintReasonId) {
        return db.run(Select.from(ComplaintReasonMappings_.class).where(b->b.ID().eq(complaintReasonId)));
    }
}
