namespace com.sap.ic.cmh.distributionChannel;

using { cuid } from '@sap/cds/common';
using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './dataType';

type DistributionChannel : Association to one DistributionChannels;

entity DistributionChannels  : cuid {
    distributionChannel         : DataType.DistributionChannel;
    distributionChannelName     : DataType.DistributionChannelName;
    salesOrganizationID         : SalesOrganization;   
}