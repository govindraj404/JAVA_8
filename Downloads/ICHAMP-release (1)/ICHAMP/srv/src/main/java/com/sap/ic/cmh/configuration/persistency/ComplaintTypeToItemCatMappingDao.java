package com.sap.ic.cmh.configuration.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings_;

@Repository
public class ComplaintTypeToItemCatMappingDao {

	@Autowired
	PersistenceService db;
    
	/**
	 * Fetch identifier based on which sequence number will be generated
	 * @return
	 */
	public Result getComplaintTypeToItemCatMappingIdentifier() {
		return db.run(Select.from(ComplaintTypeToItemCategoryMappings_.class).columns(ComplaintTypeToItemCategoryMappings.IDENTIFIER)
				.orderBy(c -> c.get("identifier").desc()));
	}

    /**
	 * Check if record exists based on the unique fields combination
	 * @param complaintTypeToItemCategoryMappings
	 * @return
	 */
	public Result getComplaintTypeToItemCatMappingBasedOnUniqueFields(ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings) {
		return db.run(Select.from(ComplaintTypeToItemCategoryMappings_.class).columns(ComplaintTypeToItemCategoryMappings.ID)
				.where(c->c.salesOrganization_ID().eq(complaintTypeToItemCategoryMappings.getSalesOrganizationId())
						.and(c.distributionChannel_ID().eq(complaintTypeToItemCategoryMappings.getDistributionChannelId())
								.and(c.division_ID().eq(complaintTypeToItemCategoryMappings.getDivisionId())
							.and(c.complaintType_ID().eq(complaintTypeToItemCategoryMappings.getComplaintTypeId())
							.and(c.itemCategory_ID().eq(complaintTypeToItemCategoryMappings.getItemCategoryId())))))));
	}
/**
     * get ItemCategorymappings type details based on Id
     * @param {@link String} complaintReasonId
     * @public
     */
    public Result getComplaintTypeToItemCatBasedOnId(String itemCategoryMapId) {
        return db.run(Select.from(ComplaintTypeToItemCategoryMappings_.class).where(b->b.ID().eq(itemCategoryMapId)));
    }
}
