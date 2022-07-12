namespace com.sap.ic.cmh.purchaseOrganization;

using {cuid} from '@sap/cds/common';
using {com.sap.ic.cmh.companyCode.CompanyCode} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './index';

type PurchaseOrganization : Association to one PurchaseOrganizations;

@cds.search: { 
    purchaseOrganization, purchaseOrganizationName
}

entity PurchaseOrganizations : cuid {
    purchaseOrganization     : DataType.PurchaseOrganization;
    purchaseOrganizationName : DataType.PurchaseOrganizationName;
    companyCodeID            : CompanyCode;
}