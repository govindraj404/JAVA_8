namespace com.sap.ic.cmh.complaintTypeToSalesAreaMapping;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.complaintTypeConfiguration.ComplaintTypeConfiguration} from './index';
using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from '../common/index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannel} from '../common/index';
using {com.sap.ic.cmh.division.Division} from '../common/index';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';


type ComplaintTypeToSalesAreaMapping : Association to one ComplaintTypeToSalesAreaMappings;

@assert.unique : {complaintTypeToSalesAreaMapping : [
    salesOrganization,
    distributionChannel,
    division,
    complaintTypeConfiguration
]}
entity ComplaintTypeToSalesAreaMappings : cuid, managed {
    salesOrganization          : SalesOrganization;
    distributionChannel        : DistributionChannel;
    division                   : Division;
    complaintTypeConfiguration : ComplaintTypeConfiguration;
}
