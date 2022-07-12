using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeConfigurations with @(
    odata.draft.enabled : true,

    Capabilities        : {
        SearchRestrictions.Searchable : true,
        Deletable : false,
        FilterRestrictions            : {FilterExpressionRestrictions : [{
            Property           : itemCategory_ID,
            AllowedExpressions : 'MultiValue'
        }]}
    },
    Common.SemanticKey  : [identifier],
    UI                  : {
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [{
                Property   : 'createdAt',
                Descending : true
            }]
        },

        SelectionFields     : [
            code,
            description,
            individualComplaintType,
            itemCategory_ID
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
                Value             : description,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : individualComplaintType,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemCategory_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : complaintCategory_code,
                ![@UI.Importance] : #Medium
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyComplaintTypeConfigurations'
            }
        ]}
    }
);
