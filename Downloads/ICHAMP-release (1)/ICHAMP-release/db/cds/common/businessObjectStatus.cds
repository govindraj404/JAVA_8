namespace com.sap.ic.cmh.businessObjectStatus;

using {cuid,managed} from '@sap/cds/common';
using {com.sap.ic.cmh.combineBusinessObjectStatus.CombineBusinessObjectStatuses} from './index';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from '../complaint/index';

type BusinessObjectStatus : Association to one BusinessObjectStatuses;

entity BusinessObjectStatuses : cuid, managed {
        businessObjectType   : String (40);
        businessObjectStatus_code :  String (40);
        businessObjectStatus      : Association to one CombineBusinessObjectStatuses on  businessObjectStatus.businessObjectType = businessObjectType and businessObjectStatus.code   = businessObjectStatus_code;
        parent : UUID;
}
