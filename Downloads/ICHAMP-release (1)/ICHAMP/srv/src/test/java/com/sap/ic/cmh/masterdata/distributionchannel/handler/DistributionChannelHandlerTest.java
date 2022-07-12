package com.sap.ic.cmh.masterdata.distributionchannel.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.distributionchannel.service.DistributionChannelService;
import com.sap.ic.cmh.masterdata.distributionchannel.validation.DistributionChannelValidation;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;

import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.SalesOrganizations;

public class DistributionChannelHandlerTest {

	@InjectMocks
	DistributionChannelHandler distributionChannelHandler;
	@Mock
	SalesOrganizationService salesOrganizationService;
	@Mock
	DistributionChannelService distributionChannelService;
	@Mock
	private Messages messages;
	@Mock
	private Message message;
	@Mock
	private DistributionChannelValidation distributionChannelValidator;
	@Mock
	private CdsService cdsService;
	@Mock
    CdsCreateEventContext createContext;
    @Mock
    CdsUpdateEventContext updateContext;
    
    private DistributionChannels distributionChannels;
    private SalesOrganizations salesOrganizations;
    
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        distributionChannels = Struct.create(DistributionChannels.class);
        distributionChannels.setDistributionChannel("1000");
        distributionChannels.setDistributionChannelName("test");
        distributionChannels.setSalesOrganization("1000");
       
        salesOrganizations = Struct.create(SalesOrganizations.class);
        salesOrganizations.setId(UUID.randomUUID().toString());
        salesOrganizations.setSalesOrganization("1000");
    }
    
    @Test
    public void testBeforeDistributionChannelsCreate() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization())).thenReturn(salesOrganizations);
    	distributionChannelHandler.beforeDistributionChannelsCreate(distributionChannels);
    }
    
    @Test
    public void testBeforeDistributionChannelsCreateSalesOrgNull() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization())).thenReturn(null);
    	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
    	distributionChannelHandler.beforeDistributionChannelsCreate(distributionChannels);
    }
    
    @Test
    public void testOnDistributionChannelsCreate() {
    	Mockito.when(distributionChannelService.getDetailsBasedOnDistributionChannelAndSalesOrg
    			(distributionChannels.getDistributionChannel(), distributionChannels.getSalesOrganization())).thenReturn(distributionChannels);
    	distributionChannelHandler.onDistributionChannelsCreate(createContext, distributionChannels);
    	
    }
    
    @Test
    public void testOnDistributionChannelsCreateNull() {
    	Mockito.when(distributionChannelService.getDetailsBasedOnDistributionChannelAndSalesOrg
    			(distributionChannels.getDistributionChannel(), distributionChannels.getSalesOrganization())).thenReturn(null);
    	distributionChannelHandler.onDistributionChannelsCreate(createContext, distributionChannels);
    }
    @Test
    public void testBeforeDistributionChannelsUpdate() {
    	Mockito.when(distributionChannelService.getDetailsBasedOnDistributionChannelAndSalesOrg
    			(distributionChannels.getDistributionChannel(), distributionChannels.getSalesOrganization())).thenReturn(distributionChannels);
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization())).thenReturn(salesOrganizations);
    	distributionChannelHandler.beforeDistributionChannelsUpdate(distributionChannels);
    	
    	
    }
    

}
