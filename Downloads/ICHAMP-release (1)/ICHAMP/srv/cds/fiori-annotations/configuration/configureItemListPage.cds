using ConfigurationService from '../../services/index';

annotate ConfigurationService.ConfigureItems with @(
    Capabilities       : {
        Deletable                     : false,
        SearchRestrictions.Searchable : true
    },
    Common.SemanticKey : [code],
    UI                 : {
        HeaderInfo          : {
            TypeName       : '{i18n>CONFIGURE_ITEM}',
            TypeNamePlural : '{i18n>CONFIGURE_ITEMS}'
        },
        PresentationVariant : {Visualizations : ['@UI.LineItem']},
        SelectionFields     : [
            code,
            associatedApplications.code
        ],
        LineItem            : {$value : [
            {
                $Type             : 'UI.DataField',
                Value             : code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : associatedApplications.code,
                ![@UI.Importance] : #High,
                Label             : '{i18n>COMPLAINT_CATEGORY}'
            }
        ]}
    }
);
