package com.sap.ic.cmh.masterdata.address.validations;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.Addresses_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AddressValidatorImpl implements AddressValidator {

    public static final Logger logger = LoggerHelper.getLogger(AddressValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the input address detail
     *
     * @param address input address detail
     */
    @Override
    public void checkInputsSanitized(Addresses address) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Address */
        dataValidator.validateData(address.getAddress(),
                MessageKeys.ADDRESS_VALIDATION_ERROR, Addresses_.class, Addresses_::address, true);

        /* Street */
        dataValidator.validateData(address.getStreet(),
                MessageKeys.STREET_VALIDATION_ERROR, Addresses_.class, Addresses_::street);

        /* House Number */
        dataValidator.validateData(address.getHouseNumber(),
                MessageKeys.HOUSE_NUMBER_VALIDATION_ERROR, Addresses_.class, Addresses_::houseNumber);

        /* Address Line 1 */
        dataValidator.validateData(address.getAddressLine1(),
                MessageKeys.ADDRESS_LINE_VALIDATION_ERROR, Addresses_.class, Addresses_::addressLine1);

        /* Address Line 2 */
        dataValidator.validateData(address.getAddressLine2(),
                MessageKeys.ADDRESS_LINE_VALIDATION_ERROR, Addresses_.class, Addresses_::addressLine2);

        /* Address Line 3 */
        dataValidator.validateData(address.getAddressLine3(),
                MessageKeys.ADDRESS_LINE_VALIDATION_ERROR, Addresses_.class, Addresses_::addressLine3);

        /* Postal Code */
        dataValidator.validateNumber(address.getPostalCode(),
                MessageKeys.POSTAL_CODE_VALIDATION_ERROR, Addresses_.class, Addresses_::postalCode);

        /* City */
        dataValidator.validateAlphabeticData(address.getCity(),
                MessageKeys.CITY_VALIDATION_ERROR, Addresses_.class, Addresses_::city);

        /* Country */
        dataValidator.validateAlphabeticData(address.getCountry(),
                MessageKeys.COUNTRY_VALIDATION_ERROR, Addresses_.class, Addresses_::country);

        /* PO Box */
        dataValidator.validateData(address.getPoBox(),
                MessageKeys.PO_BOX_VALIDATION_ERROR, Addresses_.class, Addresses_::poBox);

        /* Email */
        dataValidator.validateEmail(address.getEmail(),
                MessageKeys.INVALID_EMAIL, Addresses_.class, Addresses_::email);

        /* Mobile Number */
        dataValidator.validateMobile(address.getMobile(),
                MessageKeys.MOBILE_VALIDATION_ERROR, Addresses_.class, Addresses_::mobile);

        /* Telephone Number */
        dataValidator.validateMobile(address.getTelephone(),
                MessageKeys.TELEPHONE_VALIDATION_ERROR, Addresses_.class, Addresses_::telephone);

        /* Fax Number */
        dataValidator.validateMobile(address.getFaxNumber(),
                MessageKeys.FAX_VALIDATION_ERROR, Addresses_.class, Addresses_::faxNumber);

        /* Extension */
        dataValidator.validateData(address.getExtension(),
                MessageKeys.EXTENSION_VALIDATION_ERROR, Addresses_.class, Addresses_::extension);

        /* Contact Person */
        dataValidator.validateAlphabeticData(address.getContactPerson(),
                MessageKeys.CONTACT_PERSON_VALIDATION_ERROR, Addresses_.class, Addresses_::contactPerson);

        /* Country Key Code */
        dataValidator.validateCountry(address.getCountryKeyCode(),
                MessageKeys.INVALID_COUNTRY_KEY_CODE, Addresses_.class, Addresses_::countryKey_code);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}
