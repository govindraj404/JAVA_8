namespace com.sap.ic.cmh.complaintTypeToItemCategoryMapping;

using {
    cuid,
    managed
} from '@sap/cds/common';


using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from '../common/index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannel} from '../common/index';
using {com.sap.ic.cmh.division.Division} from '../common/index';
using {com.sap.ic.cmh.complaintTypeConfiguration.ComplaintTypeConfiguration} from './index';
using {com.sap.ic.cmh.itemCategory.ItemCategory} from './index';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';

type ComplaintTypeToItemCategoryMapping : Association to one ComplaintTypeToItemCategoryMappings;

@assert.unique : {complaintTypeToItemCategoryMapping : [
    salesOrganization,
    distributionChannel,
    division,
    complaintType,
    itemCategory
]}
entity ComplaintTypeToItemCategoryMappings : cuid, managed {
    identifier          : DataType.Identifier;
    salesOrganization   : SalesOrganization;
    distributionChannel : DistributionChannel;
    division            : Division;
    complaintType       : ComplaintTypeConfiguration;
    itemCategory        : ItemCategory;
    isActive            : Boolean default true;
}
