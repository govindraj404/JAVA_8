using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';

service SupplierIssueProcessInternalService{
    entity Supplier8DProcesses as projection on cmh.supplierIssueProcess.SupplierIssueProcesses;
}