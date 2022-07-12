namespace com.sap.ic.cmh.sourceReferenceTypeMapping;

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
using {com.sap.ic.cmh.referenceType.ReferenceType} from './index';

type SourceReferenceTypeMapping : Association to one SourceReferenceTypeMappings;

@cds.search    : {
    identifier,
    complaintType,
    itemCategory,
    salesOrganization,
    distributionChannel,
    division,
    sourceSystem
}
@assert.unique : {sourceReferenceTypeMapping : [
    complaintType,
    itemCategory,
    salesOrganization,
    distributionChannel,
    division
]}

entity SourceReferenceTypeMappings : cuid, managed {
    identifier          : DataType.Identifier;
    complaintType       : ComplaintTypeConfiguration;
    itemCategory        : ItemCategory;
    salesOrganization   : SalesOrganization;
    distributionChannel : DistributionChannel;
    division            : Division;
    sourceSystem        : DataType.Destination;
    referenceTypes      : Composition of many SourceReferenceTypes
                              on referenceTypes.sourceReferenceTypeMapping = $self;
    isActive            : Boolean default true;
}

@assert.unique : {sourceReferenceType : [
    referenceType,
    sourceReferenceTypeMapping
]}

entity SourceReferenceTypes : cuid, managed {
    referenceType              : ReferenceType;
    sourceReferenceTypeMapping : SourceReferenceTypeMapping;
}
