package com.sap.ic.cmh.masterdata.division.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.ic.cmh.masterdata.division.model.DivisionRequest;
import com.sap.ic.cmh.masterdata.division.model.DivisionResponse;
import com.sap.ic.cmh.masterdata.division.service.DivisionService;

public class DivisionControllerTest {
	
	@InjectMocks
	DivisionController divisionController;
	@Mock
	private DivisionService divisionService;
	
	List<DivisionResponse> responseList = new ArrayList<DivisionResponse>();
	List<DivisionRequest> requestList = new ArrayList<DivisionRequest>();
	DivisionResponse response1;
	DivisionResponse response2;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		response1 = new DivisionResponse();
		response1.setMessage("test");
		response1.setRecordNo("1");
		response1.setStatus("success");
		response2 = new DivisionResponse();
		response2.setMessage("test1");
		response2.setRecordNo("2");
		response2.setStatus("success");

	}
	
	@Test
	public void deleteDistributionChannels() {
		DivisionRequest request1 = new DivisionRequest();
		request1.setSalesOrganization("1000");
		request1.setDivision("1000");
		DivisionRequest request2 = new DivisionRequest();
		request2.setSalesOrganization("0001");
		request2.setDivision("0001");
		requestList.add(request1);
		requestList.add(request2);
		response1.setSalesDivision("1000");
		response1.setSalesOrganization("1000");
		responseList.add(response1);
		response2.setSalesOrganization("0001");
		response2.setSalesDivision("0001");
		responseList.add(response2);
		when(divisionService.deleteDivisionList(requestList)).thenReturn(responseList);
		divisionController.deleteDivisionLists(requestList);
		assertEquals("1", response1.getRecordNo());
		assertEquals("success", response1.getStatus());
		assertEquals("test", response1.getMessage());
		assertEquals("1000", response1.getSalesDivision());
		assertEquals("0001", response2.getSalesOrganization());
		assertEquals("1000", request1.getDivision());
		assertEquals("0001", request2.getSalesOrganization());
		
	}

	@Test
	public void divisionController() {
		divisionController.deleteDivisionLists(null);
	}

}
