using ConfigurationService from '../../services/index';

annotate ConfigurationService.ItemCategories with @(
    odata.draft.enabled : true,
    Capabilities        : {
        SearchRestrictions.Searchable : true,
        Deletable : false,
        FilterRestrictions            : {FilterExpressionRestrictions : [{
            Property           : material_ID,
            AllowedExpressions : 'MultiValue'
        }]}
    },
    Common.SemanticKey  : [identifier],
    UI                  : {

        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            RequestAtLeast : [complaintCategory_code],
            SortOrder      : [{
                Property   : 'createdAt',
                Descending : true
            }]
        },

        SelectionFields     : [
            code,
            description,
            materialEnteredManually,
            individualComplaint,
            referenceDocumentCategory_code
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
                Value             : complaintQuantityRule_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : materialEnteredManually,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : material_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : referenceDocumentCategory_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : individualComplaint,
                ![@UI.Importance] : #High
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyItemCategories'
            }
        ]}
    }
);
