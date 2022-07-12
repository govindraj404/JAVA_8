namespace com.sap.ic.cmh.supplierIssueProcessStatus;

type SupplierIssueProcessStatus : Association to one SupplierIssueProcessStatuses;

entity SupplierIssueProcessStatuses {
    key code : String (40);
        name : localized String(60);
        sequenceNumber : Integer;
}