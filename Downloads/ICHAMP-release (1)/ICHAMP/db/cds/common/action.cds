namespace com.sap.ic.cmh.action;

using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from '../complaint/index';
using {com.sap.ic.cmh.combineBusinessObjectStatus.CombineBusinessObjectStatus} from '../complaint/index';

type Action : Association to one Actions;

entity Actions {
    key code : String (40);
        name : localized String(60);
        businessObjectType : BusinessObjectType;
        targetBusinessObjectStatus : CombineBusinessObjectStatus;
}