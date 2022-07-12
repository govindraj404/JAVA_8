package com.sap.ic.cmh.masterdata.distributionchannel.service;

import java.util.List;

import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelRequest;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelResponse;

import cds.gen.masterdataservice.DistributionChannels;

public interface DistributionChannelService {
    List<DistributionChannelResponse> deleteDistributionChannelList(List<DistributionChannelRequest> distributionChannelRequest);

	DistributionChannels getDetailsBasedOnDistributionChannelAndSalesOrg(String distributionChannel, String salesOrganization);

}