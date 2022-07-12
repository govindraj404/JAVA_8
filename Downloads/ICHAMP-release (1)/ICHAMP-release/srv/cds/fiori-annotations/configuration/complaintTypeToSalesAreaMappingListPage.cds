using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeToSalesAreaMappings with @(
    Capabilities : {SearchRestrictions.Searchable : false, },
    UI           : {LineItem : {$value : [
        {
            $Type             : 'UI.DataField',
            Value             : salesOrganization_ID,
            ![@UI.Importance] : #High
        },
        {
            $Type             : 'UI.DataField',
            Value             : distributionChannel_ID,
            Label             : '{i18n>DISTRIBUTION_CHANNEL}',
            ![@UI.Importance] : #Medium
        },
        {
            $Type             : 'UI.DataField',
            Value             : division_ID,
            Label             : '{i18n>DIVISION}',
            ![@UI.Importance] : #Medium
        }
    ]}},
      Common.SideEffects #salesArea : {
        SourceProperties : [salesOrganization_ID],
        TargetProperties : [
            'distributionChannel_ID',
            'division_ID'
        ],
        TargetEntities   : [
            salesOrganization,
            distributionChannel,
            division
        ]
    }
);
