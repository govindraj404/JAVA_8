namespace com.sap.ic.cmh.salesOrganization;

using {cuid,Currency} from '@sap/cds/common';
using {com.sap.ic.cmh.companyCode.CompanyCode} from './index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannels} from './index';
using {com.sap.ic.cmh.division.Divisions} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './dataType';
using {com.sap.ic.cmh.businessPartner.BusinessPartner} from './index';




type SalesOrganization : Association to one SalesOrganizations;

entity SalesOrganizations : cuid {
    salesOrganization                  : DataType.SalesOrganization;
    salesOrganizationName              : DataType.SalesOrganizationName;
    companyCodeID                      : CompanyCode;
    currency                           : Currency;
    distributionChannel                : Association to many DistributionChannels on distributionChannel.salesOrganizationID=$self;
    division                           : Association to many Divisions on division.salesOrganizationID=$self;
    businessPartnerID                  : BusinessPartner;
}