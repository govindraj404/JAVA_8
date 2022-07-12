using ClaimService from '../../services/index';

annotate ClaimService.CostCollectors  with @(
    UI : {
        HeaderInfo  : {
            TypeName : '{i18n>ITEMS}',
            TypeNamePlural : '{i18n>ITEMS}'
        },
        PresentationVariant : {
            Visualizations : ['@UI.LineItem']
        },
        LineItem  : [
            {
                $Type : 'UI.DataField',
                Value : subItemType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : itemType_code,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : totalCost,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : quantity,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : description,
                ![@UI.Importance] : #Medium
            }
        ]
    }
);