using ConfigurationService from '../services/configurationService';

annotate ConfigurationService.ConditionTypes with @(restrict : [
    {
        grant : ['READ'],
        to    : 'ConditionType.Read'
    },
    {
        grant : ['CREATE', 'READ', 'UPDATE', 'DELETE', 'CopyConditionTypes'],
        to    : 'ConditionType.Admin'
    }
]);

annotate ConfigurationService.ServiceMaterials with @(restrict : [
    {
        grant : ['READ'],
        to    : 'ServiceMaterial.Read'
    },
    {
        grant : ['CREATE', 'READ', 'UPDATE', 'DELETE', 'CopyServiceMaterials'],
        to    : 'ServiceMaterial.Admin'
    }
]);

annotate ConfigurationService.DestinationConfigurations with @(restrict : [
    {
        grant : ['READ'],
        to    : 'DestinationConfiguration.Read'
    },
    {
        grant : ['CREATE', 'READ', 'UPDATE', 'DELETE', 'CopyDestinationConfigurations'],
        to    : 'DestinationConfiguration.Admin'
    }
]);

annotate ConfigurationService.BusinessObjectConfigurations with @(restrict : [
    {
        grant : ['READ'],
        to    : 'BusinessObjectConfiguration.Read'
    },
    {
        grant : ['CREATE', 'READ', 'UPDATE', 'DELETE', 'CopyBusinessObjectConfigurations'],
        to    : 'BusinessObjectConfiguration.Admin'
    }
]);

annotate ConfigurationService.ClaimStatusMappings with @(restrict : [
    {
        grant : ['READ'],
        to    : 'ClaimStatusMapping.Read'
    },
    {
        grant : ['CREATE', 'READ', 'UPDATE', 'DELETE', 'CopyClaimStatusMappings'],
        to    : 'ClaimStatusMapping.Admin'
    }
]);

annotate ConfigurationService.CompanyCodes with @(restrict : [
    {
        grant : ['READ'],
        where : 'companyCode = $user.CompanyCode'
    }
]);