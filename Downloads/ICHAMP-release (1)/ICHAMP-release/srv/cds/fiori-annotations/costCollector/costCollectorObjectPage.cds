using CostCollectorService from '../../services/index';


annotate CostCollectorService.CostCollectors with @(
    UI : {
        HeaderInfo          : {
            TypeName : '{i18n>ITEMS}',
            TypeNamePlural : '{i18n>ITEMS}'
        },
        PresentationVariant : {Visualizations : ['@UI.LineItem']},
        SelectionVariant #All    : {
            Text          : '{i18n>ALL}'
        },
        SelectionVariant #TransferredToClaim : {
            Text          : '{i18n>TRANSFER_TO_CLAIM}',
            SelectOptions : [{
                $Type        : 'UI.SelectOptionType',
                PropertyName : transferToClaim,
                Ranges       : [{
                    $Type  : 'UI.SelectionRangeType',
                    Sign   : #I,
                    Option : #EQ,
                    Low    : true
                }]
            }]
        },
        LineItem : [
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
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>DELETE}',
                Action : 'CostCollectorService.Delete'
            }
        ]
    }
);