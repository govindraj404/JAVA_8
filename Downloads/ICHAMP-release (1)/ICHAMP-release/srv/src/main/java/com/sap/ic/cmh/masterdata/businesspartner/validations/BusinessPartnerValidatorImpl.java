package com.sap.ic.cmh.masterdata.businesspartner.validations;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BusinessPartnerValidatorImpl implements BusinessPartnerValidator {

    public static final Logger logger = LoggerHelper.getLogger(BusinessPartnerValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the input businessPartner detail
     *
     * @param businessPartner input businessPartner destail
     */
    @Override
    public void checkInputsSanitized(BusinessPartners businessPartner) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Business Partner Number */
        dataValidator.validateData(businessPartner.getBusinessPartnerNumber(),
                MessageKeys.BUSINESS_PARTNER_NUMBER_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::businessPartnerNumber, true);
        
        /* Vendor Code */
        dataValidator.validateData(businessPartner.getVendorCode(),
                MessageKeys.VENDOR_CODE_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::vendorCode);
        
        /* Customer Code */
        dataValidator.validateData(businessPartner.getCustomerCode(),
                MessageKeys.CUSTOMER_CODE_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::customerCode);
        
        /* Company Code */
        dataValidator.validateData(businessPartner.getCompanyCode(),
                MessageKeys.COMPANY_CODE_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::companyCode);

        /* Business Partner Name 1 */
        dataValidator.validateData(businessPartner.getBusinessPartnerName1(),
                MessageKeys.BUSINESS_PARTNER_NAME_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::businessPartnerName1);

        /* Business Partner Name 2 */
        dataValidator.validateData(businessPartner.getBusinessPartnerName2(),
                MessageKeys.BUSINESS_PARTNER_NAME_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::businessPartnerName2);

        /* Business Partner Type */
        dataValidator.validateData(businessPartner.getBusinessPartnerType(),
                MessageKeys.BUSINESS_PARTNER_TYPE_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::businessPartnerType);

        /* Address */
        dataValidator.validateData(businessPartner.getAddress(),
                MessageKeys.ADDRESS_VALIDATION_ERROR, BusinessPartners_.class, BusinessPartners_::address);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}
