namespace com.sap.ic.cmh.complaintReasonMapping;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.itemCategory.ItemCategory} from './index';
using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from '../common/index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannel} from '../common/index';
using {com.sap.ic.cmh.division.Division} from '../common/index';
using {com.sap.ic.cmh.complaintReason.ComplaintReason} from './index';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';

type ComplaintReasonMapping : Association to one ComplaintReasonMappings;

@cds.search    : {
    identifier,
    salesOrganization,
    distributionChannel,
    division,
    itemCategory,
    complaintReason
}
@assert.unique : {complaintReasonMapping : [
    salesOrganization,
    distributionChannel,
    division,
    itemCategory,
    complaintReason
]}
entity ComplaintReasonMappings : cuid, managed {
    identifier          : DataType.Identifier;
    salesOrganization   : SalesOrganization;
    distributionChannel : DistributionChannel;
    division            : Division;
    itemCategory        : ItemCategory;
    complaintReason     : ComplaintReason;
    isActive            : Boolean default true;
}
