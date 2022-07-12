using ConfigurationService from '../../services/index';

annotate ConfigurationService.ClaimStatusMappings with @(
    odata.draft.enabled : true,
    Capabilities        : {SearchRestrictions.Searchable : true, },
    Common.SemanticKey  : [identifier],
    UI                  : {
        // SelectionPresentationVariant#All : {
        //     Text                  : '{i18n>CLAIM_STATUS}',
        //     SelectionVariant : {Text : '{i18n>CLAIM_STATUS}'},
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [{
                Property   : 'code',
                Descending : true
            }]
        },
        // },
        SelectionFields     : [
            code,
            name,
            status_code
        ],
        LineItem            : {$value : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : name,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : status_code,
                ![@UI.Importance] : #High
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyClaimStatusMappings'
            }
        ]}
    }
);
