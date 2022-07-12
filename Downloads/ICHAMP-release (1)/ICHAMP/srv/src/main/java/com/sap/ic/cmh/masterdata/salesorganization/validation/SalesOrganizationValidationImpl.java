package com.sap.ic.cmh.masterdata.salesorganization.validation;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.SalesOrganizations;
import cds.gen.masterdataservice.SalesOrganizations_;

/**
 * This class validates the Sales Organization details
 */
@Component
public class SalesOrganizationValidationImpl implements SalesOrganizationValidation {

	public static final Logger logger = LoggerHelper.getLogger(SalesOrganizationValidationImpl.class);

	@Autowired
	private DataValidator dataValidator;
	@Autowired
	BusinessPartnerService businessPartnerService;
	@Autowired
	CompanyCodeService companyCodeService;
	@Autowired
    Messages messages;

	/**
	 * Method used to validate and sanitize the sales organization input details
	 * 
	 * @param salesOrganization
	 */
	@Override
	public void checkInputsSanitized(SalesOrganizations salesOrganization) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Sales Org */
		dataValidator.validateData(salesOrganization.getSalesOrganization(),MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST,SalesOrganizations_.class,SalesOrganizations_::salesOrganization);

		dataValidator.validateData(salesOrganization.getSalesOrganizationName(),MessageKeys.SALES_ORGANIZATION_NAME_VALIDATION_ERROR,SalesOrganizations_.class,SalesOrganizations_::salesOrganizationName);
		
        /* Company Code */
        dataValidator.validateData(salesOrganization.getCompanyCode(),MessageKeys.COMPANY_CODE_DOES_NOT_EXIST,SalesOrganizations_.class,SalesOrganizations_::companyCode);
       
        validateBusinessPartner(salesOrganization.getBusinessPartner());
        validateCompanyCode(salesOrganization.getCompanyCode());
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
	}

	/**
	 * Validate Business partner
	 * @param businessPartner
	 */
	public void validateBusinessPartner(String businessPartner) {
		BusinessPartners businessPartnersBasedOnNumber = businessPartnerService.getBusinessPartnersBasedOnNumber(businessPartner);
		if(null==businessPartnersBasedOnNumber) {
			messages.error(MessageKeys.BUSINESS_PARTNER_NUMBER_VALIDATION_ERROR).target("in", SalesOrganizations_.class,
					SalesOrganizations_::businessPartner);
		}
	}
	
	/**
	 * Validate Company Code
	 * @param companyCode
	 */
	public void validateCompanyCode(String companyCode) {
		CompanyCodes fetchCompanyCodesBasedOnCode = companyCodeService.fetchCompanyCodesBasedOnCode(companyCode);
		if(null==fetchCompanyCodesBasedOnCode) {
			messages.error(MessageKeys.COMPANY_CODE_DOES_NOT_EXIST).target("in", SalesOrganizations_.class,
        			SalesOrganizations_::companyCode);
		}
		
	}

}
