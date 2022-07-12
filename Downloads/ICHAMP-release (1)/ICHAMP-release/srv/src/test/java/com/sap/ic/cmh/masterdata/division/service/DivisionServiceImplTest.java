package com.sap.ic.cmh.masterdata.division.service;

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
import com.sap.ic.cmh.masterdata.division.model.DivisionRequest;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.SalesOrganizations;

public class DivisionServiceImplTest {

	@InjectMocks
	DivisionServiceImpl divisionServiceImpl;
	@Mock
	LocaleMessageHelper messageHelper;
	@Mock
	private DivisionRepository divisionRepository;
	@Mock
	SalesOrganizationService salesOrganizationService;
	@Mock
	Result result;

	Row row;
	private Optional<Row> opt;
	List<DivisionRequest> requestList = new ArrayList<>();
	 List<String> salesDivisionlList = new ArrayList<>();
	List<String> divisionIdList = new ArrayList<>();
	List<Divisions> divisionList;
	Divisions divisions;
	DivisionRequest request;
	SalesOrganizations salesOrganizations;
	List<String> salesOrgIdList = new ArrayList<>();
	List<SalesOrganizations> salesOrgList ;
	Complaints complaints;
	List<Complaints> complaintsDetailList;
	
	   @Before
	    public void beforeClass() {
	        MockitoAnnotations.openMocks(this);
	        request = new DivisionRequest();
	        request.setDivision("1000");
	        request.setSalesOrganization("1000");
	        request.setRecordNo("33");

	        divisions = Struct.create(Divisions.class);
	        divisions.setId("100");
	        divisions.setSalesOrganization("1000");
	        divisions.setSalesOrganizationIDId("1");
	        divisions.setSalesDivision("1000");
	        requestList.add(request);
	        salesDivisionlList = requestList.stream().map(DivisionRequest::getDivision).collect(Collectors.toList());
	        divisionIdList.add(divisions.getId());
	        complaints = Struct.create(Complaints.class);
	        complaints.setCompanyCodeId("100");
	        complaints.setId("222");
	        salesOrganizations = Struct.create(SalesOrganizations.class);
	        salesOrganizations.setId("1");
	        salesOrganizations.setSalesOrganization("1000");
	        salesOrgIdList.add(salesOrganizations.getId());
	        row = Struct.create(Row.class);

	    }
	   
	    @Test
	    public void testDeleteDivisionListRecordsDeleted() {
	    	divisionList = new ArrayList<>();
	    	divisionList.add(divisions);
	    	List<String> list = new ArrayList<>();
	        list.add(request.getSalesOrganization());
	        List<SalesOrganizations> listDetails= new ArrayList<>();
	        listDetails.add(salesOrganizations);
	        when(salesOrganizationService.getSalesOrganizationDetailsBasedOnCodeList(list)).thenReturn(listDetails);
	        List<Row> rowvalues = new ArrayList<>();
	        row.put("salesDivision", "1000");
	        row.put("salesOrgnizationIDId", "1");
	        opt = Optional.of(row);
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        Mockito.when(result.listOf(Divisions.class)).thenReturn(divisionList);
	        when(divisionRepository.getDivisionMap(salesDivisionlList, salesOrgIdList))
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
	        when(divisionRepository.getActiveComplaintsInSalesDivisions(divisionIdList))
	        .thenReturn(result);
	        divisionServiceImpl.deleteDivisionList(requestList);
	    }
	    
	    @Test
	    public void testGetDivisionDetailsBasedOnDivisionAndSalesOrg() {
	    	divisionList = new ArrayList<>();
	    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization()))
	        .thenReturn(salesOrganizations);
	    	List<Row> rowvalues = new ArrayList<>();
	    	row.put("salesDivision", "1000");
	        row.put("salesOrgnizationIDId", "1");
	        opt = Optional.of(row);
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        divisionList.add(divisions);
	        Mockito.when(result.listOf(Divisions.class)).thenReturn(divisionList);
	        when(divisionRepository.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisions.getSalesDivision(), 
	        		divisions.getSalesOrganizationIDId())).thenReturn(result);
	    	divisionServiceImpl.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisions.getSalesDivision(), 
	    			divisions.getSalesOrganization());
	    	
	    }
	    
	    @Test
	    public void testGetDetailsBasedOnDistributionChannelAndSalesOrgNull() {
	    	divisionList = new ArrayList<>();
	    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization()))
	        .thenReturn(salesOrganizations);
	    	List<Row> rowvalues = new ArrayList<>();
	    	row.put("salesDivision", "1000");
	        row.put("salesOrgnizationIDId", "1");
	        opt = Optional.empty();
	        rowvalues.add(row);
	        Mockito.when(result.list()).thenReturn(rowvalues);
	        Mockito.when(result.first()).thenReturn(opt);
	        Mockito.when(result.listOf(Divisions.class)).thenReturn(divisionList);
	        when(divisionRepository.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisions.getSalesDivision(), 
	        		divisions.getSalesOrganizationIDId())).thenReturn(result);
	        divisionServiceImpl.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisions.getSalesDivision(), 
	        		salesOrganizations.getSalesOrganization());
	    	
	    }

}
