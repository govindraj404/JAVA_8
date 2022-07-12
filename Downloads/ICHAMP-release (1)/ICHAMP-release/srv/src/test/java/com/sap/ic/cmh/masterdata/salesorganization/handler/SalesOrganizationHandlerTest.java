package com.sap.ic.cmh.masterdata.salesorganization.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.masterdata.salesorganization.validation.SalesOrganizationValidation;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.SalesOrganizations;

public class SalesOrganizationHandlerTest {

	@InjectMocks
	@Autowired
	SalesOrganizationHandler salesOrganizationHandler;
	@Mock
	SalesOrganizationService salesOrganizationService;
	@Mock
	private CompanyCodeService companyCodeService;
	@Mock
	BusinessPartnerService businessPartnerService;
	@Mock
	private Messages messages;
	@Mock
	Message message;
	@Mock
	private SalesOrganizationValidation salesOrganizationValidator;
	@Mock
	private CdsService cdsService;
	@Mock
	CdsCreateEventContext createContext;

	private SalesOrganizations salesOrganizations;
	private CompanyCodes companyCodes;
	private BusinessPartners businessPartners;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		companyCodes = Struct.create(CompanyCodes.class);
		companyCodes.setId(UUID.randomUUID().toString());
		companyCodes.setCompanyCode("1000");

		salesOrganizations = Struct.create(SalesOrganizations.class);
		salesOrganizations.setSalesOrganization("1000");
		salesOrganizations.setSalesOrganizationName("sales org");
		salesOrganizations.setCompanyCode("1000");
		salesOrganizations.setBusinessPartner("CATERP");
		
		businessPartners = Struct.create(BusinessPartners.class);
		businessPartners.setId("10");
		businessPartners.setBusinessPartnerNumber("CATERP");
	}

	@Test
	public void testBeforeSalesOrganizationsCreate() {
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode()))
				.thenReturn(companyCodes);
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner()))
		        .thenReturn(businessPartners);
		salesOrganizationHandler.beforeSalesOrganizationsCreate(salesOrganizations);
	}

	@Test
	public void testBeforeSalesOrganizationsNull() {
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode())).thenReturn(null);
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner()))
        .thenReturn(null);
		salesOrganizationHandler.beforeSalesOrganizationsCreate(salesOrganizations);
	}

	@Test
	public void testOnSalesOrganizationsCreate() {
		Mockito.when(salesOrganizationService
				.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization()))
				.thenReturn(salesOrganizations);
		salesOrganizationHandler.onSalesOrganizationsCreate(createContext, salesOrganizations);

	}

	@Test
	public void testOnSalesOrganizationsNull() {
		Mockito.when(salesOrganizationService
				.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization()))
				.thenReturn(null);
		salesOrganizationHandler.onSalesOrganizationsCreate(createContext, salesOrganizations);
	}

	@Test
	public void testBeforeSalesOrganizationsUpdate() {
		Mockito.when(salesOrganizationService
				.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizations.getSalesOrganization()))
				.thenReturn(salesOrganizations);
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode()))
				.thenReturn(companyCodes);
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner()))
        .thenReturn(businessPartners);
		salesOrganizationHandler.beforeSalesOrganizationsUpdate(salesOrganizations);
	}

}
