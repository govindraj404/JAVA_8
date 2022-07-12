package com.sap.ic.cmh.masterdata.distributionchannel.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelRequest;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelResponse;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * This class used to receive distribution channel details, validate and process
 * data for delete
 */
@Service
public class DistributionChannelServiceImpl implements DistributionChannelService {
	public static final Logger logger = LoggerHelper.getLogger(DistributionChannelServiceImpl.class);
	@Autowired
	LocaleMessageHelper messageHelper;
	@Autowired
	private DistributionChannelRepository distributionChannelRepository;
	@Autowired
	SalesOrganizationService salesOrganizationService;

	/**
	 * This will delete the distribution inactive records by validating with the
	 * complaints and send the records which are not deleted back to the
	 * response
	 *
	 * @param distributionChannelsRequest Request containing distribution channel
	 *                                    and sales organization
	 * @return Distribution channel Records which are not deleted
	 */
	@Override
	public List<DistributionChannelResponse> deleteDistributionChannelList(
			List<DistributionChannelRequest> distributionChannelsRequest) {
		LoggerHelper.logMethodEntry(logger, "DistributionChannelServiceImpl", "deleteDistributionChannelList");
		List<DistributionChannelResponse> distributionChannelsResponse = new ArrayList<>();
		AtomicInteger integerAtomic = new AtomicInteger();
		List<String> salesOrgList = new ArrayList<>();
		List<String> distChannelList = new ArrayList<>();
		distributionChannelsRequest.forEach(distributionChannelRequest -> {
			salesOrgList.add(distributionChannelRequest.getSalesOrganization());
			distChannelList.add(distributionChannelRequest.getDistributionChannel());
			distributionChannelRequest.setRecordNo(String.valueOf(integerAtomic.incrementAndGet()));
		});
		// Get Distribution records based on sales Org and distribution channel
		List<SalesOrganizations> salesOrganizationDetailsBasedOnCode = salesOrganizationService.getSalesOrganizationDetailsBasedOnCodeList(salesOrgList);
		List<String> salesOrgIdList=!CollectionUtils.isEmpty(salesOrganizationDetailsBasedOnCode) ? salesOrganizationDetailsBasedOnCode.stream().map(SalesOrganizations::getId).collect(Collectors.toList())
				:new ArrayList<>();
		Result result = distributionChannelRepository
				.getDistributionChannelMap(distChannelList, salesOrgIdList);
		List<DistributionChannels> distributionChannelDbList = result.first().isPresent() ?
				result.listOf(DistributionChannels.class) : new ArrayList<>();
		// Collect the db records into distribution request list
		List<DistributionChannelRequest> distributionChannelRequestDb = new ArrayList<>();
		Map<String, DistributionChannels> distributionChannelDbMap = new HashMap<>();
		if(!CollectionUtils.isEmpty(distributionChannelDbList)) {
			distributionChannelDbList.forEach(distributionChannel -> {
				DistributionChannelRequest distributionChannelRequest = new DistributionChannelRequest();
				distributionChannelRequest.setDistributionChannel(distributionChannel.getDistributionChannel());
				Optional<SalesOrganizations> findFirst = salesOrganizationDetailsBasedOnCode.stream().filter(s->s.getId().equalsIgnoreCase(distributionChannel.getSalesOrganizationIDId())).findAny();
				String salesOrg = findFirst.isPresent() ? findFirst.get().getSalesOrganization() : "";
				distributionChannel.setSalesOrganization(salesOrg);
				distributionChannelRequest.setSalesOrganization(salesOrg);
				distributionChannelRequestDb.add(distributionChannelRequest);
				distributionChannelDbMap.put(distributionChannel.getId(), distributionChannel);
			});
		}
	

		// Records which does not exist will be sent to the response
		distributionChannelsRequest.stream().filter(
				distributionChannelRequest -> !distributionChannelRequestDb.contains(distributionChannelRequest))
				.forEach(distributionChannel -> {
					DistributionChannelResponse distributionChannelResponse = new DistributionChannelResponse();
					distributionChannelResponse
							.setMessage(messageHelper.getMessage(MessageKeys.DISTRIBUTION_CHANNEL_DOES_NOT_EXIST));
					distributionChannelResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
					distributionChannelResponse.setRecordNo(distributionChannel.getRecordNo());
					distributionChannelResponse.setSalesOrganization(distributionChannel.getSalesOrganization());
					distributionChannelResponse.setDistributionChannel(distributionChannel.getDistributionChannel());
					distributionChannelsResponse.add(distributionChannelResponse);
				});
		//Check for Active complaint records
		 checkActiveComplaints(distributionChannelsRequest, distributionChannelsResponse, distributionChannelDbMap);
		LoggerHelper.logMethodExit(logger, "DistributionChannelServiceImpl", "deleteDistributionChannelList");
		return distributionChannelsResponse;
	}
    
	/**
	 * Check for Active complaint records
	 * @param distributionChannelsRequest
	 * @param distributionChannelsResponse
	 * @param distributionChannelDbMap
	 */
	public void checkActiveComplaints(List<DistributionChannelRequest> distributionChannelsRequest,
			List<DistributionChannelResponse> distributionChannelsResponse,
			Map<String, DistributionChannels> distributionChannelDbMap) {
		
        if (!CollectionUtils.isEmpty(distributionChannelDbMap)) {
            List<String> recordsToBeDeleted = new ArrayList<>();
            List<String> distributionChannelIdList = new ArrayList<>(distributionChannelDbMap.keySet());
            Result activeComplaintsResult = distributionChannelRepository.getActiveComplaintsInDistributionChannels(distributionChannelIdList);
            List<Complaints> complaintsList = activeComplaintsResult.first().isPresent() ? activeComplaintsResult.listOf(Complaints.class) : new ArrayList<>();
            final List<String> activeComplaintsDistributionChannelIdList = !CollectionUtils.isEmpty(complaintsList)? complaintsList.stream().map(Complaints::getCompanyCodeId).collect(Collectors.toList())
            		: new ArrayList<>();
            for (String distributionChannelId : distributionChannelIdList) {
            	//create the payload and delete inactive distribution Channels
                createResponseAndDeleteDistributionChannel(distributionChannelsRequest, distributionChannelsResponse,
						distributionChannelDbMap, recordsToBeDeleted, activeComplaintsDistributionChannelIdList,
						distributionChannelId);
                if (!CollectionUtils.isEmpty(recordsToBeDeleted)) {
                    distributionChannelRepository.deleteInactiveDistributionChannel(recordsToBeDeleted);
                    logger.info("DistributionChannels deleted for : {}  " , recordsToBeDeleted.toString());
                }
            }
        }
	}
    
	/**
	 * create the payload and delete inactive distribution Channels
	 * @param distributionChannelsRequest
	 * @param distributionChannelsResponse
	 * @param distributionChannelDbMap
	 * @param recordsToBeDeleted
	 * @param activeComplaintsDistributionChannelIdList
	 * @param distributionChannelId
	 */
	public void createResponseAndDeleteDistributionChannel(List<DistributionChannelRequest> distributionChannelsRequest,
			List<DistributionChannelResponse> distributionChannelsResponse,
			Map<String, DistributionChannels> distributionChannelDbMap, List<String> recordsToBeDeleted,
			final List<String> activeComplaintsDistributionChannelIdList, String distributionChannelId) {
		if (!activeComplaintsDistributionChannelIdList.contains(distributionChannelId)) {
		    distributionChannelsRequest.stream()
		            .filter(e -> e.getDistributionChannel().equals(distributionChannelDbMap.get(distributionChannelId).getDistributionChannel())
		                    && e.getSalesOrganization().equals(distributionChannelDbMap.get(distributionChannelId).getSalesOrganization())
		            ).findFirst().ifPresent(distributionChannel -> {
		        //Collect records to be deleted
		        recordsToBeDeleted.add(distributionChannelId);
		        DistributionChannelResponse distributionChannelResponse = new DistributionChannelResponse();
		        distributionChannelResponse.setMessage(messageHelper.getMessage(MessageKeys.DISTRIBUTION_CHANNEL_SUCCESSFULLY_DELETED));
		        distributionChannelResponse.setStatus(messageHelper.getMessage(MessageKeys.SUCCESS));
		        distributionChannelResponse.setRecordNo(distributionChannel.getRecordNo());
		        distributionChannelResponse.setSalesOrganization(distributionChannel.getSalesOrganization());
		        distributionChannelResponse.setDistributionChannel(distributionChannel.getDistributionChannel());
		        distributionChannelsResponse.add(distributionChannelResponse);
		    });
		} else {
		    distributionChannelsRequest.stream()
		            .filter(e -> e.getDistributionChannel().equals(distributionChannelDbMap.get(distributionChannelId).getDistributionChannel())
		                    && e.getSalesOrganization().equals(distributionChannelDbMap.get(distributionChannelId).getSalesOrganization())
		            ).findFirst().ifPresent(distributionChannel -> {
		        DistributionChannelResponse distributionChannelResponse = new DistributionChannelResponse();
		        distributionChannelResponse.setMessage(messageHelper.getMessage(MessageKeys.DISTRIBUTION_CHANNEL_ASSOCIATION_TO_COMPLAINT));
		        distributionChannelResponse.setStatus(messageHelper.getMessage(MessageKeys.ERROR));
		        distributionChannelResponse.setRecordNo(distributionChannel.getRecordNo());
		        distributionChannelResponse.setSalesOrganization(distributionChannel.getSalesOrganization());
		        distributionChannelResponse.setDistributionChannel(distributionChannel.getDistributionChannel());
		        distributionChannelsResponse.add(distributionChannelResponse);
		    });
		}
	}
    
	/**
	 * Fetch DistributionChannels details based on DistributionChannels
	 * and Sales Organization
	 */
	@Override
	public DistributionChannels getDetailsBasedOnDistributionChannelAndSalesOrg(String distributionChannel,
			String salesOrganization) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getDetailsBasedOnDistributionChannelAndSalesOrg");
		SalesOrganizations salesOrganizationDetailsBasedOnSalesOrgCode = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganization);
		String salesOrgId = null!=salesOrganizationDetailsBasedOnSalesOrgCode?salesOrganizationDetailsBasedOnSalesOrgCode.getId() : "";
		Result distributionChannelsResult = distributionChannelRepository
				.getDetailsBasedOnDistributionChannelAndSalesOrg(distributionChannel, salesOrgId);
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getDetailsBasedOnDistributionChannelAndSalesOrg");
		return distributionChannelsResult.first().isPresent()
				? distributionChannelsResult.listOf(DistributionChannels.class).get(0)
				: null;
	}
}
