using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.supplier8D.dataType as datatype} from '../../../db/cds/supplier8D/index';

service SupplierIssueProcessService{
    entity Supplier8DProcesses as projection on cmh.supplierIssueProcess.SupplierIssueProcesses{
    *,
    @Core.Computed : false 3 as isSupplierFieldControl : datatype.FieldControl,
    @Core.Computed : false false as isUpdateRestricted  : datatype.UpdateRestricted,
    '' as number : datatype.Identifier
    };
}
