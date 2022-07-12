using SupplierIssueProcessService from '../services/supplier8DService';

annotate SupplierIssueProcessService.SupplierIssueProcesses with @(restrict : [
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