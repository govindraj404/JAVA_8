package com.sap.ic.cmh.masterdata.distributionchannel.persistency;

import java.util.List;

import com.sap.cds.Result;

public interface DistributionChannelRepository {

    void deleteInactiveDistributionChannel(List<String> recordsToBeDeleted);

    Result getDistributionChannelMap(List<String> distributionChannels, List<String> salesOrgList);

	Result getDetailsBasedOnDistributionChannelAndSalesOrg(String distributionChannel, String salesOrganization);
	
	Result getActiveComplaintsInDistributionChannels(List<String> distributionChannelId);

    public Result getDistributionChannelById(String distributionChannelId);

}