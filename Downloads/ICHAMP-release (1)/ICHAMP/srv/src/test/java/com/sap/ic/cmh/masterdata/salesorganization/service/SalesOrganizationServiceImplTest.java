package com.sap.ic.cmh.masterdata.salesorganization.service;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.salesorganization.model.SalesOrganizationRequest;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.SalesOrganizations;

public class SalesOrganizationServiceImplTest {
	@InjectMocks
	SalesOrganizationServiceImpl salesOrganizationServiceImpl;
	@Mock
	LocaleMessageHelper messageHelper;
	@Mock
	private SalesOrganizationRepository salesOrganizationRepository;
	
	@Mock
	Result result;

	Row row;
	private Optional<Row> opt;
	
	List<SalesOrganizationRequest> requestList = new ArrayList<>();
	 List<String> salesOrganizationlList = new ArrayList<>();
	List<String> salesOrgIdList = new ArrayList<>();
	List<SalesOrganizations> salesOrgDetailList;
	SalesOrganizations salesOrganizations;
	Complaints complaints;
	List<Complaints> complaintsDetailList;
	SalesOrganizationRequest request;
	
	
	   @Before
	    public void beforeClass() {
	        MockitoAnnotations.openMocks(this);
	        request = new SalesOrganizationRequest();
	        request.setSalesOrganization("1000");
	        request.setRecordNo("33");

	        salesOrganizations = Struct.create(SalesOrganizations.class);
	        salesOrganizations.setId("100");
	        salesOrganizations.setSalesOrganization("1000");
	        requestList.add(request);
	        salesOrganizationlList = requestList.stream().map(SalesOrganizationRequest::getSalesOrganization).collect(Collectors.toList());
	        salesOrgIdList.add(salesOrganizations.getId());
	        
	        complaints = Struct.create(Complaints.class);
	        complaints.setCompanyCodeId("100");
	        complaints.setId("222");
	        row = Struct.create(Row.class);

	    }
	   @Test
	    public void testSalesOrganizationListRecordsDeleted() {
	    	salesOrgDetailList = new ArrayList<>();
	    	salesOrgDetailList.add(salesOrganizations);
	        List<Row> rowvalues = new ArrayList<>();
	        row.put("salesOrganization", "1000");
	        opt = Optional.of(row);
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        Mockito.when(result.listOf(SalesOrganizations.class)).thenReturn(salesOrgDetailList);
	        when(salesOrganizationRepository.getSalesOrganizationMap(salesOrganizationlList))
	        .thenReturn(result);
	        complaintsDetailList=new ArrayList<>();
	        complaintsDetailList.add(complaints);
	        List<Row> rowvaluesComplaints = new ArrayList<>();
	        row.put("companyCodeId", "100");
	        opt = Optional.of(row);
	        rowvaluesComplaints.add(row);
	        Mockito.when(result.list()).thenReturn(rowvaluesComplaints);
	        Mockito.when(result.first()).thenReturn(opt);
	        Mockito.when(result.listOf(Complaints.class)).thenReturn(complaintsDetailList);
	        when(salesOrganizationRepository.getActiveComplaintsInSalesOrganizations(salesOrgIdList))
	        .thenReturn(result);
	        salesOrganizationServiceImpl.deleteSalesOrganizationList(requestList);
	    }
	    
	    @Test
	    public void testGetSalesOrganizationDetailsBasedOnSalesOrgCode() {
	    	salesOrgDetailList = new ArrayList<>();
	    	List<Row> rowvalues = new ArrayList<>();
	        row.put("salesOrganization", "ComplaintID");
	        opt = Optional.of(row);
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        salesOrgDetailList.add(salesOrganizations);
	        Mockito.when(result.listOf(SalesOrganizations.class)).thenReturn(salesOrgDetailList);
	        when(salesOrganizationRepository.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization())).thenReturn(result);
	        salesOrganizationServiceImpl.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization());
	    }
	    
	    @Test
	    public void testGetSalesOrganizationDetailsBasedOnSalesOrgCodeNull() {
	    	salesOrgDetailList = new ArrayList<>();
	    	List<Row> rowvalues = new ArrayList<>();
	        row.put("salesOrganization", "ComplaintID");
	        opt = Optional.empty();
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        Mockito.when(result.listOf(SalesOrganizations.class)).thenReturn(salesOrgDetailList);
	        when(salesOrganizationRepository.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization())).thenReturn(result);
	        salesOrganizationServiceImpl.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization());
	    }
	    
	    @Test
	    public void testGetSalesOrganizationDetailsBasedOnCodeList() {
	    	List<SalesOrganizations> detailList =new ArrayList<>();
	    	List<String> list = new ArrayList<>();
	        list.add(request.getSalesOrganization());
	    	List<Row> rowvalues = new ArrayList<>();
	        row.put("salesOrganization", "ComplaintID");
	        opt = Optional.of(row);
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        detailList.add(salesOrganizations);
	        Mockito.when(result.listOf(SalesOrganizations.class)).thenReturn(detailList);
	        when(salesOrganizationRepository.getSalesOrganizationDetailsBasedOnCodeList(list)).thenReturn(result);
	        salesOrganizationServiceImpl.getSalesOrganizationDetailsBasedOnCodeList(list);
	        
	    }
	    
	    @Test
	    public void testGetSalesOrganizationDetailsBasedOnCodeListNull() {
	    	List<SalesOrganizations> detailList =new ArrayList<>();
	    	List<String> list = new ArrayList<>();
	        list.add(request.getSalesOrganization());
	    	List<Row> rowvalues = new ArrayList<>();
	        row.put("salesOrganization", "ComplaintID");
	        opt = Optional.empty();
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        detailList.add(salesOrganizations);
	        Mockito.when(result.listOf(SalesOrganizations.class)).thenReturn(detailList);
	        when(salesOrganizationRepository.getSalesOrganizationDetailsBasedOnCodeList(list)).thenReturn(result);
	        salesOrganizationServiceImpl.getSalesOrganizationDetailsBasedOnCodeList(list);
	        
	    }

}
