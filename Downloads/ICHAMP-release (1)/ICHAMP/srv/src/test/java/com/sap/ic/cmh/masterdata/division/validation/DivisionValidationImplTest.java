package com.sap.ic.cmh.masterdata.division.validation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.SalesOrganizations;

public class DivisionValidationImplTest {
	
	@InjectMocks
	DivisionValidationImpl divisionValidationImpl;
	@Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;
    @Mock
	private Message message;
    @Mock
    SalesOrganizationService salesOrganizationService;
    
    private Divisions divisions;
    SalesOrganizations salesOrganizations;
    
    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        divisions = Struct.create(Divisions.class);
        divisions.setSalesDivision("1000");
        divisions.setSalesDivisionName("test");
        divisions.setSalesOrganization("1000");
        
        salesOrganizations = Struct.create(SalesOrganizations.class);
        salesOrganizations.setId("22224");
        salesOrganizations.setSalesOrganization("1000");
        
    }
    
    @Test
    public void testCheckInputsSanitized() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisions.getSalesOrganization())).thenReturn(salesOrganizations);
    	divisionValidationImpl.checkInputsSanitized(divisions);
    	
    }
    
    @Test
    public void testCheckInputsSanitizedNullSalesOrg() {
    	when(salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisions.getSalesOrganization())).thenReturn(null);
    	Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        divisionValidationImpl.checkInputsSanitized(divisions);
    	
    }

}
