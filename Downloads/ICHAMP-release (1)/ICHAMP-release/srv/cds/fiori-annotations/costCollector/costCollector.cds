using CostCollectorService from '../../services/index';

annotate CostCollectorService.CostCollectors with {
    ID
    @UI.Hidden;

    subItemType
    @Common.Label : '{i18n>SUB_ITEM_TYPE}';

    itemType
    @Common.Label : '{i18n>ITEM_TYPE}';

    description
    @UI.MultiLineText
    @Common.Label : '{i18n>DESCRIPTION}';

    currency_code
    @Common.Label : '{i18n>CURRENCY}';

    totalCost
    @Measures.ISOCurrency : currency_code
    @Common.Label         : '{i18n>TOTAL_COST}';
    
    quantity
    @Measures.Unit : unit_code
    @Common.Label : '{i18n>QUANTITY}';

    unit
    @Common.Label         : '{i18n>UNIT}';

    subItemType_code
    @Common.Label : '{i18n>SUB_ITEM_TYPE}';

    itemType_code
    @Common.Label : '{i18n>ITEM_TYPE}';

    unit_code
    @Common.Label         : '{i18n>UNIT}';

    parent
    @UI.Hidden;

    claim
    @UI.Hidden;

    transferToClaim
    @UI.Hidden;
}