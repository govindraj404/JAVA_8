package com.sap.ic.cmh.masterdata.division.handler;

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
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.division.service.DivisionService;
import com.sap.ic.cmh.masterdata.division.validation.DivisionValidation;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;

import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.SalesOrganizations;

public class DivisionHandlerTest {
	
	@InjectMocks
	DivisionHandler divisionHandler;
	
	@Mock
    SalesOrganizationService salesOrganizationService;
	@Mock
    DivisionService divisionService;
	@Mock
    private Messages messages;
	@Mock
	private Message message;
	@Mock
    private DivisionValidation divisionValidator;
	@Mock
	private CdsService cdsService;
	@Mock
    CdsCreateEventContext createContext;
	
	private Divisions divisions;
    private SalesOrganizations salesOrganizations;
    
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        divisions = Struct.create(Divisions.class);
        divisions.setSalesDivision("1000");
        divisions.setSalesDivisionName("test");
        divisions.setSalesOrganization("1000");
       
        salesOrganizations = Struct.create(SalesOrganizations.class);
        salesOrganizations.setId(UUID.randomUUID().toString());
        salesOrganizations.setSalesOrganization("1000");
    }
    
    @Test
    public void testBeforeDivisionsCreate() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisions.getSalesOrganization())).thenReturn(salesOrganizations);
    	divisionHandler.beforeDivisionsCreate(divisions);
    }
    
    @Test
    public void testBeforeDivisionsCreateSalesOrgNull() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisions.getSalesOrganization())).thenReturn(null);
    	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        divisionHandler.beforeDivisionsCreate(divisions);
    }
    
    @Test
    public void testOnDivisionsCreate() {
    	Mockito.when(divisionService.getDivisionDetailsBasedOnDivisionAndSalesOrg
    			(divisions.getSalesDivision(), divisions.getSalesOrganization())).thenReturn(divisions);
    	divisionHandler.onDivisionsCreate(createContext, divisions);
    	
    }
    
    @Test
    public void testOnDivisionsCreateNull() {
    	Mockito.when(divisionService.getDivisionDetailsBasedOnDivisionAndSalesOrg
    			(divisions.getSalesDivision(), divisions.getSalesOrganization())).thenReturn(null);
    	divisionHandler.onDivisionsCreate(createContext, divisions);
    }
    @Test
    public void testBeforeDivisionsUpdate() {
    	Mockito.when(divisionService.getDivisionDetailsBasedOnDivisionAndSalesOrg
    			(divisions.getSalesDivision(), divisions.getSalesOrganization())).thenReturn(divisions);
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisions.getSalesOrganization())).thenReturn(salesOrganizations);
    	divisionHandler.beforDivisionsUpdate(divisions);
    	
    	
    }
    

}
