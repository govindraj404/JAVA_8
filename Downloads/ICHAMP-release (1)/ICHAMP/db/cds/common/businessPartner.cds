namespace com.sap.ic.cmh.businessPartner;

using {cuid} from '@sap/cds/common';
using {com.sap.ic.cmh.address.Address} from './index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from './index';
using {com.sap.ic.cmh.common.dataType as DataType} from './index';
using {com.sap.ic.cmh.salesOrganization.SalesOrganizations} from './index';

type BusinessPartner : Association to one BusinessPartners;

@cds.search: { 
    businessPartnerNumber, businessPartnerName1
}
entity BusinessPartners  : cuid {
    businessPartnerNumber   : DataType.BusinessPartnerNumber;
    vendorCode              : DataType.VendorCode;
    customerCode            : DataType.CustomerCode;
    companyCodeID           : CompanyCode;
    businessPartnerName1    : DataType.BusinessPartnerName1;
    businessPartnerName2    : DataType.BusinessPartnerName2;
    addressID               : Address;
    isMarkedForDeletion     : DataType.IsMarkedForDeletion default false;
    businessPartnerType     : DataType.BusinessPartnerType;
    salesOrganization       : Association to many SalesOrganizations on salesOrganization.businessPartnerID=$self;
}