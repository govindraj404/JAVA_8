namespace com.sap.ic.cmh.businessObjectRelation;

using {cuid,managed} from '@sap/cds/common';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from '../complaint/index';
using com.sap.ic.cmh.combineBusinessObject.CommonBusinessObjects from '../common/index';

type BusinessObjectRelation : Association to one BusinessObjectRelations;

entity BusinessObjectRelations : cuid, managed {
        sourceBusinessObjectType   : BusinessObjectType;
        targetBusinessObjectType   : BusinessObjectType;
        sourceBusinessObjectUUID   :  UUID;
        targetBusinessObjectUUID   : Association to CommonBusinessObjects;
}
