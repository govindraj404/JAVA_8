namespace com.sap.ic.cmh.businessObjectAttribute;

using {com.sap.ic.cmh.businessObjectType.BusinessObjectTypes} from '../complaint/index';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';

type BusinessObjectAttribute : Association to one BusinessObjectAttributes;

entity BusinessObjectAttributes {
    key businessObjectAttribute : DataType.BusinessObjectAttribute;
    key businessObjectType      : DataType.BusinessObjectType;
        name                    : localized DataType.Name;
        businessObjectType_code : Association to one BusinessObjectTypes
                                      on businessObjectType_code.code = businessObjectType;
}
