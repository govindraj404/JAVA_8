using QualityNotificationService from '../../services/index';

annotate QualityNotificationService.QualityNotifications with @(
    UI : {
        LineItem : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            }
        ]
    }
); 