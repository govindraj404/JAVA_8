namespace com.sap.ic.cmh.division;

using { cuid } from '@sap/cds/common';
using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './dataType';

type Division : Association to one Divisions;

entity Divisions : cuid {

    salesDivision              : DataType.SalesDivision;
    salesDivisionName          : DataType.SalesDivisionName;
    salesOrganizationID        : SalesOrganization;
}