using ConfigurationService from '../../services/index';

annotate ConfigurationService.ConditionTypes with @(
    odata.draft.enabled : true,

    Capabilities        : {
        SearchRestrictions.Searchable : true,
        FilterRestrictions            : {FilterExpressionRestrictions : [{
            Property           : destination,
            AllowedExpressions : 'MultiValue'
        }]}
    },
    Common.SemanticKey  : [identifier],
    UI                  : {
        // SelectionPresentationVariant#All : {
        //     Text                  : '{i18n>CONDITION_TYPES}',
        //     SelectionVariant : {Text : '{i18n>CONDITION_TYPES}'},
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [
                {
                    Property   : 'destination',
                    Descending : true
                },
                {
                    Property   : 'identifier',
                    Descending : true
                }
            ]
        },
        //},
        SelectionFields     : [
            identifier,
            destination,
            itemType_code,
            conditionType
        ],
        LineItem            : {$value : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : destination,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : conditionType,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : description,
                ![@UI.Importance] : #Medium
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyConditionTypes'
            }
        ]}
    }
);
