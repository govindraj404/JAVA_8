package com.sap.ic.cmh.configuration.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import cds.gen.configurationservice.SourceReferenceTypeMappings_;
import com.sap.cds.ql.cqn.CqnSelectListItem;
import java.util.ArrayList;
import java.util.List;
import com.sap.cds.ql.CQL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.sap.ic.cmh.utils.LoggerHelper;

@Repository
public class SourceReferenceTypeMappingDao {

    @Autowired
    PersistenceService db;

    /**
     * Get source reference type mappings
     *
     * @public
     */
    private static final Logger logger = LoggerFactory.getLogger(SourceReferenceTypeMappingDao.class);
    private static final String SOURCE_REFERENCE_TYPE_MAPPING_DAO = "SourceReferenceTypeMappingDao";
    public Result getSourceReferenceTypeMappings() {
        return db.run(Select.from(SourceReferenceTypeMappings_.class).orderBy(c -> c.get("identifier").desc()));
    }

    /**
     * Get source reference type mappings based on values
     *
     * @public
     */
    public Result getSourceReferenceTypeMappingBasedOnValues(SourceReferenceTypeMappings mapping){
        return db.run(Select.from(SourceReferenceTypeMappings_.class).where(b -> b.complaintType_ID()
                .eq(mapping.getComplaintTypeId())
                .and(b.distributionChannel_ID().eq(mapping.getDistributionChannelId()))
                .and(b.division_ID().eq(mapping.getDivisionId()))
                .and(b.itemCategory_ID().eq(mapping.getItemCategoryId()))
                .and(b.salesOrganization_ID().eq(mapping.getSalesOrganizationId()))
                .and(b.sourceSystem().eq(mapping.getSourceSystem()))));
    }
    /**
     *
     * fetch SourceReferenceTypeMapping based on ID.
     *
     * @public
     */
    public Result getSourceReferenceTypeMappingBasedOnId(String sourceReferenceId) {
        LoggerHelper.logMethodEntry(logger, SOURCE_REFERENCE_TYPE_MAPPING_DAO, "getSourceReferenceTypeMappingBasedOnId");
        List<CqnSelectListItem> selectList = new ArrayList<>();
        selectList.add(CQL.to("referenceTypes").expand());
        selectList.add(CQL.star());
        return db.run(Select.from(SourceReferenceTypeMappings_.class)
                .columns(selectList)
                .where(b->b.ID().eq(sourceReferenceId)));
    }
}