using ConfigurationService from '../../services/index';

annotate ConfigurationService.BusinessObjectConfigurations with @(
    odata.draft.enabled : true,
    Capabilities        : {SearchRestrictions.Searchable : true, },
    Common.SemanticKey  : [identifier],
    UI                  : {
        // SelectionPresentationVariant#All : {
        //     Text                  : '{i18n>BUSINESS_OBJECTS}',
        //     SelectionVariant : {Text : '{i18n>BUSINESS_OBJECTS}'},
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [
                {
                    Property   : 'destination',
                    Descending : true
                },
                {
                    Property   : 'complaintType_code',
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
            complaintType_code,
            businessObjectType_code,
            businessObjectAttribute_code,
            businessObjectValue
        ],
        LineItem            : {

        $value : [
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
                Value             : complaintType_code,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : businessObjectType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : businessObjectAttribute_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : businessObjectValue,
                ![@UI.Importance] : #Medium
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopyBusinessObjectConfigurations'
            }
        ]}
    }
);
