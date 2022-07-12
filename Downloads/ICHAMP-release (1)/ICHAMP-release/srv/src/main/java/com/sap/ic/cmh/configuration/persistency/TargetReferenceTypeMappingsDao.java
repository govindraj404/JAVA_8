package com.sap.ic.cmh.configuration.persistency;

import com.sap.cds.ql.CQL;
import com.sap.cds.ql.cqn.CqnSelectListItem;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypeMappings_;

import java.util.ArrayList;
import java.util.List;

@Repository
public class TargetReferenceTypeMappingsDao {

    @Autowired
    PersistenceService db;

    private static final Logger logger = LoggerFactory.getLogger(TargetReferenceTypeMappingsDao.class);
    private static final String TARGET_REFERENCE_TYPE_MAPPING_DAO = "TargetReferenceTypeMappingsDao";


    /**
     * Get source reference type mappings
     *
     * @public
     */
    public Result getTargetReferenceTypeMappings() {
        return db.run(Select.from(TargetReferenceTypeMappings_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**
     * Get source reference type mappings based on values
     *
     * @public
     */
    public Result getTargetReferenceTypeMappingBasedOnValues(TargetReferenceTypeMappings mapping){
        return db.run(Select.from(TargetReferenceTypeMappings_.class).where(b -> b.complaintType_ID()
                .eq(mapping.getComplaintTypeId())
                .and(b.distributionChannel_ID().eq(mapping.getDistributionChannelId()))
                .and(b.division_ID().eq(mapping.getDivisionId()))
                .and(b.itemCategory_ID().eq(mapping.getItemCategoryId()))
                .and(b.salesOrganization_ID().eq(mapping.getSalesOrganizationId()))));
    }
    /**
     * To get Target Reference Type Based on ID
     * @param {link String} id
     * @public
     */
    public Result getTargetReferenceTypeMappingDetailsBasedOnID(String id){
        LoggerHelper.logMethodEntry(logger, TARGET_REFERENCE_TYPE_MAPPING_DAO, "-getTargetReferenceTypeMappingDetailsBasedOnID");

        List<CqnSelectListItem> selectList = new ArrayList<>();
        selectList.add(CQL.to("targetTypes").expand());
        selectList.add(CQL.star());
        LoggerHelper.logMethodExit(logger, TARGET_REFERENCE_TYPE_MAPPING_DAO, "getTargetReferenceTypeMappingDetailsBasedOnID");

        return db.run(Select.from(TargetReferenceTypeMappings_.class)
                .columns(selectList)
                .where(b->b.ID().eq(id)));
    }
}