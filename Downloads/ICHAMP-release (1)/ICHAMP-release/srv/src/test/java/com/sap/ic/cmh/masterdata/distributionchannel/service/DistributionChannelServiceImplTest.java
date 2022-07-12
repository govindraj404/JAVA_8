package com.sap.ic.cmh.masterdata.distributionchannel.service;

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
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.distributionchannel.model.DistributionChannelRequest;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.SalesOrganizations;

public class DistributionChannelServiceImplTest {
	
	
	@InjectMocks
	@Autowired
	DistributionChannelServiceImpl distributionChannelServiceImpl;
	@Mock
	LocaleMessageHelper messageHelper;
	@Mock
	DistributionChannelRepository distributionChannelRepository;
	@Mock
	SalesOrganizationService salesOrganizationService;
	@Mock
	Result result;
	
	Row row;
	private Optional<Row> opt;
	List<DistributionChannelRequest> requestList = new ArrayList<>();
	private List<String> distributionChannelDetailList = new ArrayList<>();
	List<String> distributionChannelIdList = new ArrayList<>();
	List<DistributionChannels> distributionChannelList;
	DistributionChannels distributionChannels;
	DistributionChannelRequest request;
	
	Complaints complaints;
	List<Complaints> complaintsDetailList;
	SalesOrganizations salesOrganizations;
	List<String> salesOrgIdList ;
	List<SalesOrganizations> salesOrgList ;
	
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        request = new DistributionChannelRequest();
        request.setDistributionChannel("1000");
        request.setSalesOrganization("1000");
        request.setRecordNo("33");

        distributionChannels = Struct.create(DistributionChannels.class);
        distributionChannels.setId("100");
        distributionChannels.setDistributionChannel("1000");
        distributionChannels.setSalesOrganization("1000");
        distributionChannels.setSalesOrganizationIDId("1");
        requestList.add(request);
        distributionChannelDetailList = requestList.stream().map(DistributionChannelRequest::getDistributionChannel).collect(Collectors.toList());
        distributionChannelIdList.add(distributionChannels.getId());
        complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("100");
        complaints.setId("222");
        salesOrganizations = Struct.create(SalesOrganizations.class);
        salesOrgIdList = new ArrayList<>();
        salesOrgList = new ArrayList<>();
        salesOrganizations.setId("1");
        salesOrganizations.setSalesOrganization("1000");
        salesOrgIdList.add(salesOrganizations.getId());
        salesOrgList.add(salesOrganizations);
        row = Struct.create(Row.class);
    }
    
    @Test
    public void testDeleteDistributionChannelList1() {
    	distributionChannelList = new ArrayList<>();
    	 distributionChannelList.add(distributionChannels);
        List<String> list = new ArrayList<>();
        list.add(request.getSalesOrganization());
        List<SalesOrganizations> listDetails= new ArrayList<>();
        listDetails.add(salesOrganizations);
        when(salesOrganizationService.getSalesOrganizationDetailsBasedOnCodeList(list)).thenReturn(listDetails);
        List<Row> rowvalues = new ArrayList<>();
        row.put("distributionChannel", "1000");
        row.put("salesOrganizationIDId", "1");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelList);
        when(distributionChannelRepository.getDistributionChannelMap(distributionChannelDetailList, salesOrgIdList))
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
        when(distributionChannelRepository.getActiveComplaintsInDistributionChannels(distributionChannelIdList))
        .thenReturn(result);
        distributionChannelServiceImpl.deleteDistributionChannelList(requestList);
    }
    
    @Test
    public void testGetDetailsBasedOnDistributionChannelAndSalesOrg() {
    	distributionChannelList = new ArrayList<>();
    	salesOrganizations.setId("1");
    	distributionChannels.setSalesOrganizationIDId("1");
    	distributionChannels.setSalesOrganization("1000");
    	 distributionChannelList.add(distributionChannels);
        when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization()))
        .thenReturn(salesOrganizations);
    	List<Row> rowvalues = new ArrayList<>();
        row.put("salesOrganization", "1000");
        row.put("salesOrganizationIDId", "1");
        row.put("distributionChannel", "1000");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelList);
        when(distributionChannelRepository.getDetailsBasedOnDistributionChannelAndSalesOrg(distributionChannels.getDistributionChannel(), 
        		distributionChannels.getSalesOrganizationIDId())).thenReturn(result);
    	distributionChannelServiceImpl.getDetailsBasedOnDistributionChannelAndSalesOrg(distributionChannels.getDistributionChannel(), 
        		salesOrganizations.getSalesOrganization());
    	
    }
    
    @Test
    public void testGetDetailsBasedOnDistributionChannelAndSalesOrgNull() {
    	distributionChannelList = new ArrayList<>();
    	salesOrganizations.setId("1");
    	salesOrganizations.setSalesOrganization("1000");
    	distributionChannels.setSalesOrganizationIDId("1");
    	distributionChannels.setSalesOrganization("1000");
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization()))
        .thenReturn(salesOrganizations);
    	List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "ComplaintID");
        opt = Optional.empty();
        rowvalues.add(row);
        Mockito.when(result.list()).thenReturn(rowvalues);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelList);
        when(distributionChannelRepository.getDetailsBasedOnDistributionChannelAndSalesOrg(distributionChannels.getDistributionChannel(), 
        		distributionChannels.getSalesOrganizationIDId())).thenReturn(result);
    	distributionChannelServiceImpl.getDetailsBasedOnDistributionChannelAndSalesOrg(distributionChannels.getDistributionChannel(), 
    			salesOrganizations.getSalesOrganization());
    	
    }
    
    

}
