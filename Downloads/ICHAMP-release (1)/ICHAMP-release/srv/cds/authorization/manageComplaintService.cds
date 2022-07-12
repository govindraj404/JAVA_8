using ManageComplaintService from '../services/manageComplaintService';

annotate ManageComplaintService.Complaints with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'Complaint.Create'
    },
    {
        grant : ['READ'],
        to    : 'Complaint.Read',
        where : 'plant.plant = $user.Plant AND companyCode.companyCode = $user.CompanyCode'
    },
    {
        grant : ['UPDATE'],
        to    : 'Complaint.Update',
        where : 'plant.plant = $user.Plant AND companyCode.companyCode = $user.CompanyCode'
    }
]);

annotate ManageComplaintService.Streams with @(restrict : [
    {
        grant : ['READ'],
        to    : 'Stream.Read'
    },
    {
        grant : ['UPDATE'],
        to    : 'Stream.Update'
    }
]);

annotate ManageComplaintService.CostCollectors with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'CostCollector.Create'
    },
    {
        grant : ['READ'],
        to    : 'CostCollector.Read'
    },
    {
        grant : ['UPDATE'],
        to    : 'CostCollector.Update'
    }
]);