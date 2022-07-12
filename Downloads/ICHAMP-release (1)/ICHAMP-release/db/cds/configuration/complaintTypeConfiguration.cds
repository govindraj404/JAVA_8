namespace com.sap.ic.cmh.complaintTypeConfiguration;

using {
    cuid,
    managed
} from '@sap/cds/common';

using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.itemCategory.ItemCategory} from './index';
using {com.sap.ic.cmh.complaintTypeToSalesAreaMapping.ComplaintTypeToSalesAreaMappings} from './index';
using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from '../common/index';

type ComplaintTypeConfiguration : Association to one ComplaintTypeConfigurations;

@cds.search    : {
    identifier,
    code,
    description
}
@assert.unique : {code : [code]}
entity ComplaintTypeConfigurations : cuid, managed {
    identifier                       : DataType.Identifier;
    complaintCategory                : ComplaintCategory;
    code                             : DataType.Code;
    description                      : DataType.Description;
    individualComplaintType          : DataType.Flag;
    itemCategory                     : ItemCategory;
    complaintTypeToSalesAreaMappings : Composition of many ComplaintTypeToSalesAreaMappings
                                           on complaintTypeToSalesAreaMappings.complaintTypeConfiguration = $self;
    isActive                         : Boolean default true;
}
