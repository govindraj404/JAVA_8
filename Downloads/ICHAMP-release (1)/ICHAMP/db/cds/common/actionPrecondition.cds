namespace com.sap.ic.cmh.actionPrecondition;

using {com.sap.ic.cmh.action.Action} from './index';
using {com.sap.ic.cmh.combineBusinessObjectStatus.CombineBusinessObjectStatus} from './index';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from '../complaint/index';

type ActionPrecondition : Association to one ActionPreconditions;

entity ActionPreconditions {
    key code                 : Action;
        businessObjectType   : BusinessObjectType;
        businessObjectStatus : CombineBusinessObjectStatus;
}