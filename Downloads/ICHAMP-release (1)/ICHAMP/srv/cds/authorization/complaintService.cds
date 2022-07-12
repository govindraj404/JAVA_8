using ComplaintService from '../services/complaintService';

annotate ComplaintService.Complaints with @(restrict : [
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
        to    : 'Complaint.Update'
    },
    {
        grant : ['Discard'],
        to    : 'Complaint.Discard'
    },
    {
        grant : ['Reopen'],
        to    : 'Complaint.Reopen'
    },
    {
        grant : ['Close'],
        to    : 'Complaint.Close'
    },
]);

annotate ComplaintService.Streams with @(restrict : [
    {
        grant : ['READ'],
        to    : 'Stream.Read'
    },
    {
        grant : ['UPDATE'],
        to    : 'Stream.Update'
    }
]);

annotate ComplaintService.Plants with @(restrict : [
    {
        grant : ['READ'],
        where : 'plant = $user.Plant AND companyCodeID.companyCode = $user.CompanyCode'
    }
]);

annotate ComplaintService.CompanyCodes with @(restrict : [
    {
        grant : ['READ'],
        where : 'companyCode = $user.CompanyCode'
    }
]);

annotate ComplaintService.CostCollectors with @(restrict : [
    {
        grant : ['READ'],
        to    : 'CostCollector.Read'
    },
    {
        grant : ['UPDATE'],
        to    : 'CostCollector.Update'
    }
]);