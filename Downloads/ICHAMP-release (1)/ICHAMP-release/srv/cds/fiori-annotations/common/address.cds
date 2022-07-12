using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.address.Addresses with {
    city
    @Common.Label  : '{i18n>CITY}';
    postalCode
    @Common.Label  : '{i18n>POSTAL_CODE}';
    houseNumber
    @Common.Label  : '{i18n>HOUSE_NUMBER}';
    address
    @Common.Label  : '{i18n>ADDRESS}';
    mobile
    @Common.Label  : '{i18n>MOBILE}';
    email
    @Common.Label  : '{i18n>EMAIL}';
    country
    @Common.Label  : '{i18n>COUNTRY_NAME}';
}