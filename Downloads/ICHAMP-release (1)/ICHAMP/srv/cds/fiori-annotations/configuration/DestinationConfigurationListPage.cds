using ConfigurationService from '../../services/index';

annotate ConfigurationService.DestinationConfigurations with @(
    odata.draft.enabled : true,
    Capabilities        : {SearchRestrictions.Searchable : true, },
    Common.SemanticKey  : [identifier],
    UI                  : {
        // SelectionPresentationVariant#All : {
        //     Text                  : '{i18n>DESTINATIONS}',
        //     SelectionVariant : {Text : '{i18n>DESTINATIONS}'},
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [
                {
                    Property   : 'companyCode_ID',
                    Descending : true
                },
                {
                    Property   : 'businessObjectType_code',
                    Descending : true
                },
                {
                    Property   : 'identifier',
                    Descending : true
                }
            ]
        },
        // },
        SelectionFields     : [
            destination,
            companyCode_ID,
            description,
        ],
        LineItem            : {$value : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : companyCode_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : businessObjectType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : destination,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : navigationDestination,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : description,
                ![@UI.Importance] : #Medium
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyDestinationConfigurations'
            }
        ]}
    }
);
