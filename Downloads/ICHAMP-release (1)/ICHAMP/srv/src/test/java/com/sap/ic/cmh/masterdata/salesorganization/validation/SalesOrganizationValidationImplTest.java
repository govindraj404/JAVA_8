package com.sap.ic.cmh.masterdata.salesorganization.validation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.SalesOrganizations;

public class SalesOrganizationValidationImplTest {

	@InjectMocks
	@Autowired
	SalesOrganizationValidationImpl salesOrganizationValidationImpl;

	@Mock
	private DataValidator dataValidator;

	@Mock
	private LocaleMessageHelper messageHelper;
	@Mock
	private Messages messages;
	@Mock
	private Message message;
	@Mock
	BusinessPartnerService businessPartnerService;
	@Mock
	CompanyCodeService companyCodeService;

	SalesOrganizations salesOrganizations;
	CompanyCodes companyCodes;
	BusinessPartners businessPartners;

	@Before
	public void beforeClass() throws Exception {
		MockitoAnnotations.openMocks(this);
		salesOrganizations = Struct.create(SalesOrganizations.class);
		salesOrganizations.setId("22224");
		salesOrganizations.setSalesOrganization("1000");
		salesOrganizations.setSalesOrganizationName("sales org name");
		salesOrganizations.setCompanyCode("1000");
		salesOrganizations.setBusinessPartner("CATERP");
		
		companyCodes = Struct.create(CompanyCodes.class);
		companyCodes.setCompanyCode("1000");
		
		businessPartners = Struct.create(BusinessPartners.class);
		businessPartners.setBusinessPartnerNumber("CATERP");
		
	}

	@Test
	public void testCheckInputsSanitized() {
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner())).thenReturn(businessPartners);
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode())).thenReturn(companyCodes);
		salesOrganizationValidationImpl.checkInputsSanitized(salesOrganizations);
	}
	
	@Test
	public void testCheckInputsSanitizedBPNull() {
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner())).thenReturn(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode())).thenReturn(companyCodes);
		salesOrganizationValidationImpl.checkInputsSanitized(salesOrganizations);
	}
	
	@Test
	public void testCheckInputsSanitizedCompanyCodeNull() {
		when(businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizations.getBusinessPartner())).thenReturn(businessPartners);
		when(companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizations.getCompanyCode())).thenReturn(null);
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
		salesOrganizationValidationImpl.checkInputsSanitized(salesOrganizations);
	}

}
