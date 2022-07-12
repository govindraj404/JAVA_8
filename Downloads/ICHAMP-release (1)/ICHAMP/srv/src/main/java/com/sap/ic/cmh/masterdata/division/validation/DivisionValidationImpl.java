package com.sap.ic.cmh.masterdata.division.validation;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.Divisions_;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * This class validates the division details
 */
@Component
public class DivisionValidationImpl implements DivisionValidation {

    public static final Logger logger = LoggerHelper.getLogger(DivisionValidationImpl.class);

    @Autowired
    private DataValidator dataValidator;
    @Autowired
    SalesOrganizationService salesOrganizationService;
    @Autowired
    Messages messages;

    /**
     * Method used to validate the division input details
     * @param division
     */
    @Override
    public void checkInputsSanitized(Divisions division) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
        dataValidator.validateData(division.getSalesDivision(), MessageKeys.SALES_DIVISION_DOES_NOT_EXIST, Divisions_.class, Divisions_::salesDivision);
        dataValidator.validateData(division.getSalesDivisionName(), MessageKeys.SALES_DIVISION_NAME_VALIDATION_ERROR, Divisions_.class, Divisions_::salesDivisionName);
        dataValidator.validateData(division.getSalesOrganization(), MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST, Divisions_.class, Divisions_::salesOrganization);
        validateSalesOrganization(division.getSalesOrganization());
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
    
    /**
     * Validate Sales Organization
     * @param salesOrganization
     */
	public void validateSalesOrganization(String salesOrganization) {
		SalesOrganizations salesOrganizationDetails = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganization);
		if(null==salesOrganizationDetails) {
			messages.error(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST).target("in", Divisions_.class,
					Divisions_::salesOrganization);
		}
	}
}

