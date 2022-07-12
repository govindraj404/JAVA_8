package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintChannels;
import com.sap.cds.Result;

public interface ComplaintChannelService {

    public Result getAllComplaintChannelsOrderByIdentifier();

    public ComplaintChannels getComplaintChannelDetails(String id);
}
