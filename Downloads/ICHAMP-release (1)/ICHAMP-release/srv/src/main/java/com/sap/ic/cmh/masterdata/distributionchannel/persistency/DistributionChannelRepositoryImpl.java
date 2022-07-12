package com.sap.ic.cmh.masterdata.distributionchannel.persistency;

import static cds.gen.masterdataservice.MasterDataService_.DISTRIBUTION_CHANNELS;

import java.util.List;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.DistributionChannels_;

@Component
public class DistributionChannelRepositoryImpl implements DistributionChannelRepository {
    public static final Logger logger = LoggerHelper.getLogger(DistributionChannelRepositoryImpl.class);
    
    private static final String DISTRIBUTION_CHANNEL_REPO_IMPL = "DistributionChannelRepositoryImpl";

    @Autowired
    private PersistenceService db;

    @Override
    public void deleteInactiveDistributionChannel(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, DISTRIBUTION_CHANNEL_REPO_IMPL, "deleteInactiveDistributionChannel");
        CqnDelete delete = Delete.from(DISTRIBUTION_CHANNELS).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Distributionchannels deleted count : {}", deleteCount);
        LoggerHelper.logMethodExit(logger, DISTRIBUTION_CHANNEL_REPO_IMPL, "deleteInactiveDistributionChannel");
    }

    @Override
    public Result getDistributionChannelMap(List<String> distributionChannels, List<String> salesOrgIdList) {
        LoggerHelper.logMethodEntry(logger, DISTRIBUTION_CHANNEL_REPO_IMPL, "getDistributionChannelMap");
        CqnSelect distributionChannelSelect = Select.from(DistributionChannels_.class).columns(DistributionChannels.ID,
        		DistributionChannels.DISTRIBUTION_CHANNEL,DistributionChannels.SALES_ORGANIZATION_ID_ID)
                .where(b -> b.distributionChannel().in(distributionChannels).and(
                        b.salesOrganizationID_ID().in(salesOrgIdList)));
        LoggerHelper.logMethodExit(logger, DISTRIBUTION_CHANNEL_REPO_IMPL, "getDistributionChannelMap");
        return db.run(distributionChannelSelect);

    }

	@Override
	public Result getDetailsBasedOnDistributionChannelAndSalesOrg(String distributionChannel,
			String salesOrganizationId) {
		 CqnSelect select = Select.from(DISTRIBUTION_CHANNELS).where(b -> b.distributionChannel().eq(distributionChannel).and(
	                b.salesOrganizationID_ID().eq(salesOrganizationId)
	        ));
		return db.run(select);
	}
    
	/**
	 * Get active customer complaints with a distribution channel
	 */
	@Override
	public Result getActiveComplaintsInDistributionChannels(List<String> distributionChannelId) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInDistributionChannels");
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.companyCode_ID()
        		.in(distributionChannelId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInDistributionChannels");
		return db.run(complaintsSelect);
	}

    /**
     * Fetch distribution channel details based on ID
	 */
    @Override
    public Result getDistributionChannelById(String distributionChannelId){
        return db.run(Select.from(DistributionChannels_.class)
                .columns(DistributionChannels.SALES_ORGANIZATION_ID_ID,DistributionChannels.ID).where(b->b.ID().eq(distributionChannelId)));
    }

}