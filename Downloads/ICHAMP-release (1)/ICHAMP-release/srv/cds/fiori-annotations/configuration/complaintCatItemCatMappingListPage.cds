using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintCatItemCatMappings with @(
    odata.draft.enabled : true,
    Capabilities        : {
        SearchRestrictions.Searchable : true,
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
            complaintCategory_code,
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
                Value             : complaintCategory_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemCategory_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyComplaintCatItemCatMappings'
            }
        ]}
    }
);
