namespace com.sap.ic.cmh.targetReferenceTypeMapping;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from '../common/index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannel} from '../common/index';
using {com.sap.ic.cmh.division.Division} from '../common/index';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.itemCategory.ItemCategory} from './index';
using {com.sap.ic.cmh.complaintTypeConfiguration.ComplaintTypeConfiguration} from './index';
using {com.sap.ic.cmh.targetType.TargetType} from './index';
using {com.sap.ic.cmh.refundControl.RefundControl} from './index';


type TargetReferenceTypeMapping : Association to one TargetReferenceTypeMappings;

@cds.search    : {
    identifier,
    complaintType,
    itemCategory,
    salesOrganization,
    distributionChannel,
    division
}

@assert.unique : {targetReferenceTypeMapping : [
    complaintType,
    itemCategory,
    salesOrganization,
    distributionChannel,
    division
]}
entity TargetReferenceTypeMappings : cuid, managed {
    identifier          : DataType.Identifier;
    complaintType       : ComplaintTypeConfiguration;
    itemCategory        : ItemCategory;
    salesOrganization   : SalesOrganization;
    distributionChannel : DistributionChannel;
    division            : Division;   
    targetTypes         : Composition of many TargetReferenceTypes
                              on targetTypes.targetReferenceTypeMapping = $self;
    isActive            : Boolean default true;
}

@assert.unique : {targetReferenceType : [
    targetType,
    targetReferenceTypeMapping
]}
entity TargetReferenceTypes : cuid, managed {
    targetType                 : TargetType;
    destinationSystem          : DataType.Destination;
    targetReferenceTypeMapping : TargetReferenceTypeMapping;
    targetItemCategoryCode     : String(4) ;
}
