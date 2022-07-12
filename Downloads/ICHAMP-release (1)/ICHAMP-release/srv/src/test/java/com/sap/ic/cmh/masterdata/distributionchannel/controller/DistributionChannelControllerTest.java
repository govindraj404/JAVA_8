package com.sap.ic.cmh.masterdata.distributionchannel.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelRequest;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelResponse;
import com.sap.ic.cmh.masterdata.distributionchannel.service.DistributionChannelService;

public class DistributionChannelControllerTest {

	@InjectMocks
	DistributionChannelController distributionChannelController;
	@Mock
	private DistributionChannelService distributionChannelService;

	List<DistributionChannelResponse> responseList = new ArrayList<DistributionChannelResponse>();
	List<DistributionChannelRequest> requestList = new ArrayList<DistributionChannelRequest>();
	DistributionChannelResponse response1;
	DistributionChannelResponse response2;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		response1 = new DistributionChannelResponse();
		response1.setMessage("test");
		response1.setRecordNo("1");
		response1.setStatus("success");
		response2 = new DistributionChannelResponse();
		response2.setMessage("test1");
		response2.setRecordNo("2");
		response2.setStatus("success");

	}

	@Test
	public void deleteDistributionChannels() {
		DistributionChannelRequest request1 = new DistributionChannelRequest();
		request1.setSalesOrganization("1000");
		request1.setDistributionChannel("1000");
		DistributionChannelRequest request2 = new DistributionChannelRequest();
		request2.setSalesOrganization("0001");
		request2.setDistributionChannel("0001");
		requestList.add(request1);
		requestList.add(request2);
		response1.setDistributionChannel("1000");
		response1.setSalesOrganization("1000");
		responseList.add(response1);
		response2.setSalesOrganization("0001");
		response2.setDistributionChannel("0001");
		responseList.add(response2);
		when(distributionChannelService.deleteDistributionChannelList(requestList)).thenReturn(responseList);
		distributionChannelController.deleteDistributionChannelLists(requestList);
		assertEquals("1", response1.getRecordNo());
		assertEquals("success", response1.getStatus());
		assertEquals("test", response1.getMessage());
		assertEquals("1000", response1.getDistributionChannel());
		assertEquals("0001", response2.getSalesOrganization());
		assertEquals("1000", request1.getDistributionChannel());
		assertEquals("0001", request2.getSalesOrganization());
		
	}

	@Test
	public void deleteDistributionChannelsNull() {
		distributionChannelController.deleteDistributionChannelLists(null);
	}

}
