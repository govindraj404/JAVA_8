using ClaimService from '../services/claimService';

annotate ClaimService.Claims with @(restrict : [
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