package com.sap.ic.cmh.masterdata.distributionchannel.validation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.SalesOrganizations;

public class DistributionChannelValidationImplTest {
	
	@InjectMocks
	@Autowired
	DistributionChannelValidationImpl distributionChannelValidationImpl;
	@Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;
    @Mock
	private Message message;
    @Mock
    SalesOrganizationService salesOrganizationService;
    
    private DistributionChannels distributionChannels;
    SalesOrganizations salesOrganizations;
    
    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        distributionChannels = Struct.create(DistributionChannels.class);
        distributionChannels.setDistributionChannel("1000");
        distributionChannels.setDistributionChannelName("test");
        distributionChannels.setSalesOrganization("1000");
        
        salesOrganizations = Struct.create(SalesOrganizations.class);
        salesOrganizations.setId("22224");
        salesOrganizations.setSalesOrganization("1000");
        
    }
    
    @Test
    public void testCheckInputsSanitized() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization())).thenReturn(salesOrganizations);
    	distributionChannelValidationImpl.checkInputsSanitized(distributionChannels);
    	
    }
    
    @Test
    public void testCheckInputsSanitizedNullSalesOrg() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannels.getSalesOrganization())).thenReturn(null);
    	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
    	distributionChannelValidationImpl.checkInputsSanitized(distributionChannels);
    	
    }
	
	

}
