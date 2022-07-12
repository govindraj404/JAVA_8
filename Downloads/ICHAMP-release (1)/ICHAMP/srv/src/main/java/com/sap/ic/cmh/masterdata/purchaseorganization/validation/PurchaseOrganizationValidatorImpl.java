package com.sap.ic.cmh.masterdata.purchaseorganization.validation;

import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PurchaseOrganizationValidatorImpl implements PurchaseOrganizationValidator {

    public static final Logger logger = LoggerHelper.getLogger(PurchaseOrganizationValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    @Override
    public void checkInputsSanitized(PurchaseOrganizations purchaseOrganizations) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Purchase Organization */
        dataValidator.validateData(purchaseOrganizations.getPurchaseOrganization(),
                MessageKeys.PURCHASE_ORGANIZATION_VALIDATION_ERROR, PurchaseOrganizations_.class, PurchaseOrganizations_::purchaseOrganization, true);

        /* Purchase Organization Name */
        dataValidator.validateData(purchaseOrganizations.getPurchaseOrganizationName(),
                MessageKeys.PURCHASE_ORGANIZATION_NAME_VALIDATION_ERROR, PurchaseOrganizations_.class, PurchaseOrganizations_::purchaseOrganizationName);

        /* Company Code */
        dataValidator.validateData(purchaseOrganizations.getCompanyCode(),
                MessageKeys.COMPANY_CODE_VALIDATION_ERROR, PurchaseOrganizations_.class, PurchaseOrganizations_::companyCode);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }

}
