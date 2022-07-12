package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintChannels;
import com.sap.cds.Result;
import com.sap.ic.cmh.configuration.persistency.ComplaintChannelDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ComplaintChannelServiceImpl implements ComplaintChannelService{

    public static final Logger logger = LoggerHelper.getLogger(ComplaintChannelServiceImpl.class);
    private static final String COMPLAINT_CHANNEL_SERVICE_IMPL = "ComplaintChannelServiceImpl";

    @Autowired
    ComplaintChannelDao complaintChannelDao;

    /**
     *
     * fetch all complaint channel config based on identifier column.
     *
     * @public
     */
    @Override
    public Result getAllComplaintChannelsOrderByIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_SERVICE_IMPL, "getAllComplaintChannelsID");
        return complaintChannelDao.getAllComplaintChannelsOrderByIdentifier();
    }

    /**
     *
     * fetch complaint channel config based on ID.
     *
     * @public
     */
    @Override
    public ComplaintChannels getComplaintChannelDetails(String id) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_SERVICE_IMPL, "getComplaintChannelDetails");
        Result complaintChannelResult = complaintChannelDao.getComplaintChannelDetailsBasedOnId(id);
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_SERVICE_IMPL, "getComplaintChannelDetails");
        return complaintChannelResult.first().isPresent() ? complaintChannelResult.listOf(ComplaintChannels.class).get(0):null;
    }
}
