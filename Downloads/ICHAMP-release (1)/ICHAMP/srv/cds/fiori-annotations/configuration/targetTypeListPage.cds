using ConfigurationService from '../../services/index';

annotate ConfigurationService.TargetTypes with @(
    odata.draft.enabled : true,
    Capabilities        : {
        SearchRestrictions.Searchable : true,
        Deletable : false
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
            targetDocumentCategory_code
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
                Value             : targetDocumentCategory_code,
                ![@UI.Importance] : #High
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyTargetTypes'
            }
        ]}
    }
);
