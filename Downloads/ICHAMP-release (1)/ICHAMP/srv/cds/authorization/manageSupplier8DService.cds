using ManageSupplierIssueProcessService from '../services/manageSupplier8DService';

annotate ManageSupplierIssueProcessService.SupplierIssueProcesses with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'SupplierIssueProcess.Create'
    },
    {
        grant : ['READ'],
        to    : 'SupplierIssueProcess.Read',
        where : 'plant.plant = $user.Plant AND company.companyCode = $user.CompanyCode'
    },
    {
        grant : ['UPDATE'],
        to    : 'SupplierIssueProcess.Update'
    }
]);