using ConfigurationService from '../../services/index';

annotate ConfigurationService.ServiceMaterials with @(
    odata.draft.enabled : true,
    Capabilities        : {SearchRestrictions.Searchable : true, },
    Common.SemanticKey  : [identifier],
    UI                  : {
        // SelectionPresentationVariant#All : {
        //     Text                  : '{i18n>SERVICE_MATERIALS}',
        //     SelectionVariant : {Text : '{i18n>SERVICE_MATERIALS}'},
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [
                {
                    Property   : 'destination',
                    Descending : true
                },
                {
                    Property   : 'itemType_code',
                    Descending : true
                },
                {
                    Property   : 'subItemType_code',
                    Descending : true
                },
                {
                    Property   : 'identifier',
                    Descending : true
                }
            ]
        },
        //  },
        SelectionFields     : [
            destination,
            itemType_code,
            subItemType_code,
            serviceMaterial,
            description
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
                Value             : subItemType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemType_code,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : serviceMaterial,
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
                Action : 'ConfigurationService.CopyServiceMaterials'
            }
        ]}
    }
);
