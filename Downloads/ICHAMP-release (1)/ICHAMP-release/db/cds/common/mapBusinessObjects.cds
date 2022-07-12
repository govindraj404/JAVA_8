namespace com.sap.ic.cmh.mapBusinessObjects;

using {com.sap.ic.cmh.claim.Claims} from '../claim/index';
using {com.sap.ic.cmh.qualityNotification.QualityNotifications} from '../qualityNotification/index';
using {com.sap.ic.cmh.returnPurchaseOrder.ReturnPurchaseOrders} from '../returnPurchaseOrder/index';
using {com.sap.ic.cmh.supplierIssueProcess.SupplierIssueProcesses} from '../supplier8D/index';
using {com.sap.ic.cmh.combineBusinessObject.CommonBusinessObjects} from './index';

type BusinessObjectMapping : Association to one BusinessObjectMappings;
view BusinessObjectMappings as
    select from Claims {
      key ID                     as ID,
          identifier             as identifier,
          businessObjectStatuses as businessObjectStatuses,
          businessObjectStatuses.businessObjectType,
          businessObjectStatuses.businessObjectStatus_code,
          personResponsible as personResponsible,
          complaint as complaint,
          createdAt as createdAt,
           createdBy as createdBy
       
    }
  union
    select from QualityNotifications {
      key ID                     as ID,
          identifier             as identifier,
            businessObjectStatuses as businessObjectStatuses,
          businessObjectStatuses.businessObjectType,
          businessObjectStatuses.businessObjectStatus_code,
          personResponsible      as personResponsible,
          complaint              as complaint,
          createdAt              as createdAt,
          createdBy              as createdBy
        
    }
    union
    select from SupplierIssueProcesses {
      key ID                     as ID,
          identifier             as identifier,
            businessObjectStatuses as businessObjectStatuses,
          businessObjectStatuses.businessObjectType,
          businessObjectStatuses.businessObjectStatus_code,
          personResponsible      as personResponsible,
          complaint              as complaint,
          createdAt              as createdAt,
          createdBy              as createdBy
      
    }union
    select from ReturnPurchaseOrders {
      key ID                     as ID,
          identifier             as identifier,
           businessObjectStatuses as businessObjectStatuses,
          businessObjectStatuses.businessObjectType,
          businessObjectStatuses.businessObjectStatus_code,
         personResponsible      as personResponsible,
          complaint              as complaint,
          createdAt              as createdAt,
          createdBy              as createdBy
      
    };

