using ManageClaimService from '../services/manageClaimService';

annotate ManageClaimService.Claims with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'Claim.Create'
    },
    {
        grant : ['READ'],
        to    : 'Claim.Read',
        where : 'plant.plant = $user.Plant AND company.companyCode = $user.CompanyCode'
    },
    {
        grant : ['UPDATE'],
        to    : 'Claim.Update'
    }
]);