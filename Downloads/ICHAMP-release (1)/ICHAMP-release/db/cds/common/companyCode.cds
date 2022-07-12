namespace com.sap.ic.cmh.companyCode;

using {cuid,Country,Currency} from '@sap/cds/common';
using {com.sap.ic.cmh.address.Address} from './index';
using {com.sap.ic.cmh.common.dataType as DataType} from './index';

type CompanyCode : Association to one CompanyCodes;

@cds.search: { 
    companyCode, companyCodeName
}
entity CompanyCodes : cuid {
    companyCode     : DataType.CompanyCode;
    companyCodeName : DataType.CompanyCodeName;
    countryKey      : Country;
    currency        : Currency;
    addressID       : Address;
}