namespace com.sap.ic.cmh.businessObject;

using {cuid,managed } from '@sap/cds/common';
using {com.sap.ic.cmh.stream.Stream} from './index';
using com.sap.ic.cmh.combineBusinessObject.CommonBusinessObjects from '../common/index';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from './index';

type BusinessObject : Association to one BusinessObjects;

entity BusinessObjects : cuid, managed {
        businessObjectType : BusinessObjectType;
        isRelevant         : Boolean;
        businessObjectID   : Association to CommonBusinessObjects;
        stream             : Stream;
        complaint          : UUID;
}