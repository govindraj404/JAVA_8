namespace com.sap.ic.cmh.combineBusinessObject;

using {com.sap.ic.cmh.claim.Claims} from '../claim/index';
using {com.sap.ic.cmh.qualityNotification.QualityNotifications} from '../qualityNotification/index';
using {com.sap.ic.cmh.returnPurchaseOrder.ReturnPurchaseOrders} from '../returnPurchaseOrder/index';
using {com.sap.ic.cmh.supplierIssueProcess.SupplierIssueProcesses} from '../supplier8D/index';

type CommonBusinessObject : Association to one CommonBusinessObjects;
view CommonBusinessObjects as
select from Claims {
   key ID                     as ID,
   identifier             as identifier,
   businessObjectStatuses as businessObjectStatuses,
   personResponsible as personResponsible,
   supplier as supplier,
   contactPerson as contactPerson,
   complaint as complaint,
   createdAt as createdAt,
   createdBy as createdBy,
   status
}
union
select from QualityNotifications {
   key ID                     as ID,
   identifier             as identifier,
   businessObjectStatuses as businessObjectStatuses,
   personResponsible      as personResponsible,
   supplier as supplier,
   contactPerson as contactPerson,
   complaint              as complaint,
   createdAt              as createdAt,
   createdBy              as createdBy,
   status

}
union
select from SupplierIssueProcesses {
   key ID                     as ID,
   identifier             as identifier,
   businessObjectStatuses as businessObjectStatuses,
   personResponsible      as personResponsible,
   supplier as supplier,
   contactPerson as contactPerson,
   complaint              as complaint,
   createdAt              as createdAt,
   createdBy              as createdBy,
   status

}union
select from ReturnPurchaseOrders {
   key ID                     as ID,
   identifier             as identifier,
   businessObjectStatuses as businessObjectStatuses,
   personResponsible      as personResponsible,
   supplier as supplier,
   contactPerson as contactPerson,
   complaint              as complaint,
   createdAt              as createdAt,
   createdBy              as createdBy,
   status
};