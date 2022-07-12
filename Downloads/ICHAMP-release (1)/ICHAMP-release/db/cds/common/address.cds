namespace com.sap.ic.cmh.address;

using {cuid, Country} from '@sap/cds/common';

using com.sap.ic.cmh.common.dataType as DataType from './index';

type Address : Association to one Addresses;

entity Addresses : cuid{
    address         : DataType.Address;
    street          : DataType.Street;
    houseNumber     : DataType.HouseNumber;
    addressLine1    : DataType.AddressLine1;
    addressLine2    : DataType.AddressLine2;
    addressLine3    : DataType.AddressLine3;
    postalCode      : DataType.PostalCode;
    city            : DataType.City;
    countryKey      : Country;
    country         : DataType.Country;
    poBox           : DataType.PoBox;
    mobile          : DataType.Mobile;
    telephone       : DataType.Telephone;
    extension       : DataType.Extension;
    faxNumber       : DataType.FaxNumber;
    email           : DataType.Email;
    contactPerson   : DataType.ContactPerson;
}