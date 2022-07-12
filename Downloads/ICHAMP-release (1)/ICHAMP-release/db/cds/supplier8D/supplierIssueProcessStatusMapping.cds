namespace com.sap.ic.cmh.supplierIssueProcessStatusMapping;

using {com.sap.ic.cmh.supplierIssueProcessStatus.SupplierIssueProcessStatus} from '../supplier8D/index';

type SupplierIssueProcessStatusMapping : Association to one SupplierIssueProcessStatusMappings;

entity SupplierIssueProcessStatusMappings {
    key code : String (10);
    key fieldName : String(5);
        status : SupplierIssueProcessStatus;
}