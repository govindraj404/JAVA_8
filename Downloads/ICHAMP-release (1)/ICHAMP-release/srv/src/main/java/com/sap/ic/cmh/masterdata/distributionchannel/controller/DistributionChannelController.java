package com.sap.ic.cmh.masterdata.distributionchannel.controller;



import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelRequest;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelResponse;
import com.sap.ic.cmh.masterdata.distributionchannel.service.DistributionChannelService;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * This class used to receive Distribution Channel details for delete
 *
 */
@RestController
@RequestMapping ("/api/MasterDataService")
public class DistributionChannelController {

    public static final Logger logger = LoggerHelper.getLogger(DistributionChannelController.class);

    @Autowired
    private DistributionChannelService distributionChannelService;

    /**
     * Method takes as distribution channel request as an input for deleting the distribution channel
     *
     * @param distributionChannelsRequest
     * @return List of Distribution Channel responses which are nor deleted
     */
    @DeleteMapping ("/DistributionChannels")
    public ResponseEntity<List<DistributionChannelResponse>> deleteDistributionChannelLists(@RequestBody List<DistributionChannelRequest> distributionChannelsRequest) {
        LoggerHelper.logMethodEntry(logger, "DistributionChannelController", "deleteDistributionChannelLists");
        final List<DistributionChannelResponse> distributionChannelResponse = distributionChannelService.deleteDistributionChannelList(distributionChannelsRequest);
        LoggerHelper.logMethodExit(logger, "DistributionChannelController", "deleteDistributionChannelLists");
        return new ResponseEntity<>(distributionChannelResponse, HttpStatus.ACCEPTED);
    }

}