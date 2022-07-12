using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.common.dataType as DataType} from '../../../db/cds/common/index';
using {com.sap.ic.cmh.complaint.dataType as Dt} from '../../../db/cds/complaint/index';

service ManageSupplierIssueProcessService{
    @Capabilities.Updatable : false
    @Capabilities.Deletable : false
    entity Supplier8DProcesses as projection on cmh.supplierIssueProcess.SupplierIssueProcesses{
     *,
    @Core.Computed: false '' as contactPersonCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as complaintCode : Dt.Identifier,
    @Core.Computed: false '' as personResponsibleCode : DataType.BusinessPartnerNumber
    };
}