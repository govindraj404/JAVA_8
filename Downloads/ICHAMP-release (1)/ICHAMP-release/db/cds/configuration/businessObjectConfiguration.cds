namespace com.sap.ic.cmh.businessObjectConfiguration;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from '../common/index';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectTypes} from '../complaint/index';
using {com.sap.ic.cmh.businessObjectAttribute.BusinessObjectAttributes} from './index';

@cds.search : {
    identifier,
    complaintType,
    businessObjectType,
    destination,
    businessObjectAttribute_code,
    businessObjectValue
}

entity BusinessObjectConfigurations : cuid, managed {
    identifier                   : DataType.Identifier;
    complaintType                : ComplaintCategory;
    businessObjectType_code      : String(40);
    destination                  : DataType.Destination;
    businessObjectAttribute_code : DataType.BusinessObjectAttribute;
    businessObjectAttribute      : Association to one BusinessObjectAttributes
                                       on  businessObjectAttribute.businessObjectType      = businessObjectType_code
                                       and businessObjectAttribute.businessObjectAttribute = businessObjectAttribute_code;
    businessObjectValue          : DataType.BusinessObjectValue;
}
