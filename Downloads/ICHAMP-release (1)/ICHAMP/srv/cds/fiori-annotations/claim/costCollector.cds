using ClaimService from '../../services/index';

annotate ClaimService.CostCollectors with {
    
    ID
    @UI.Hidden;

    parent_ID
    @UI.Hidden;

    parent
    @UI.Hidden;

    claim
    @UI.Hidden;

    transferToClaim
    @UI.Hidden;

    subItemType
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>SUB_ITEM_TYPE}';

    itemType
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>ITEM_TYPE}';

    description
    @Common.FieldControl : #ReadOnly
    @UI.MultiLineText
    @Common.Label : '{i18n>DESCRIPTION}';

    totalCost
    @Measures.ISOCurrency : currency_code
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>TOTAL_COST}';

    currency_code
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>CURRENCY}';

    quantity
    @Measures.Unit : unit_code
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>QUANTITY}';

    unit
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>UNIT}';

    unit_code
    @Common.Label : '{i18n>UNIT}';

    currency
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>CURRENCY}';
    
    subItemType_code
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>SUB_ITEM_TYPE}';
    
    itemType_code
    @Common.FieldControl : #ReadOnly
    @Common.Label : '{i18n>ITEM_TYPE}';

}