using QualityNotificationService from '../services/qualityNotificationService';

annotate QualityNotificationService.QualityNotifications with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'QualityNotification.Create'
    },
    {
        grant : ['READ'],
        to    : 'QualityNotification.Read',
        where : 'plant.plant = $user.Plant AND company.companyCode = $user.CompanyCode'
    },
    {
        grant : ['UPDATE'],
        to    : 'QualityNotification.Update'
    }
]);