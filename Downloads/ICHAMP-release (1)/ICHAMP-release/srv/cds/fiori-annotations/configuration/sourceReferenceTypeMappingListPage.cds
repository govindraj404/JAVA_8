using ConfigurationService from '../../services/index';

annotate ConfigurationService.SourceReferenceTypeMappings with @(
    odata.draft.enabled : true,

    Capabilities        : {
        SearchRestrictions.Searchable : true,
        Deletable                     : false,
        FilterRestrictions            : {FilterExpressionRestrictions : [
            {
                Property           : salesOrganization_ID,
                AllowedExpressions : 'MultiValue'
            },
            {
                Property           : distributionChannel_ID,
                AllowedExpressions : 'MultiValue'
            },
            {
                Property           : division_ID,
                AllowedExpressions : 'MultiValue'
            }
        ]}
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
            salesOrganization_ID,
            distributionChannel_ID,
            division_ID,
            sourceSystem
        ],
        LineItem            : {$value : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : complaintType_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemCategory_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : salesOrganization_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : distributionChannel_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : division_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'UI.DataField',
                Value             : sourceSystem,
                ![@UI.Importance] : #High
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>COPY}',
                Action : 'ConfigurationService.CopySourceReferenceTypeMappings'
            }
        ]}
    }
);
