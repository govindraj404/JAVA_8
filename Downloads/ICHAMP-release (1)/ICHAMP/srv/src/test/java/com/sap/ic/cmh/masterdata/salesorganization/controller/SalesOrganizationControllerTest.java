package com.sap.ic.cmh.masterdata.salesorganization.controller;

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
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationRequest;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationResponse;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;

public class SalesOrganizationControllerTest {
	@InjectMocks
	SalesOrganizationController salesOrganizationController;
	@Mock
	private SalesOrganizationService salesOrganizationService;
	
	List<SalesOrganizationResponse> responseList = new ArrayList<SalesOrganizationResponse>();
	List<SalesOrganizationRequest> requestList = new ArrayList<SalesOrganizationRequest>();
	SalesOrganizationResponse response1;
	SalesOrganizationResponse response2;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		response1 = new SalesOrganizationResponse();
		response1.setMessage("test");
		response1.setRecordNo("1");
		response1.setStatus("success");
		response2 = new SalesOrganizationResponse();
		response2.setMessage("test1");
		response2.setRecordNo("2");
		response2.setStatus("success");
	}
	
	@Test
	public void testDeleteSalesOrganizationLists() {
		SalesOrganizationRequest request1 = new SalesOrganizationRequest();
		request1.setSalesOrganization("1000");
		SalesOrganizationRequest request2 = new SalesOrganizationRequest();
		request2.setSalesOrganization("0001");
		requestList.add(request1);
		requestList.add(request2);
		response1.setSalesOrganization("1000");
		responseList.add(response1);
		response2.setSalesOrganization("0001");
		responseList.add(response2);
		when(salesOrganizationService.deleteSalesOrganizationList(requestList)).thenReturn(responseList);
		salesOrganizationController.deleteSalesOrganizationLists(requestList);
		assertEquals("1", response1.getRecordNo());
		assertEquals("success", response1.getStatus());
		assertEquals("test", response1.getMessage());
		assertEquals("0001", response2.getSalesOrganization());
		assertEquals("0001", request2.getSalesOrganization());
		
	}

	@Test
	public void testDeleteSalesOrganizationListsNull() {
		salesOrganizationController.deleteSalesOrganizationLists(null);
	}

}
