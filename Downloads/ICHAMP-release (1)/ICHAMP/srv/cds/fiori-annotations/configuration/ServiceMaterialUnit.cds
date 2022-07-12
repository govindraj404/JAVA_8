using ConfigurationService from '../../services/index';

annotate ConfigurationService.ServiceMaterialUnits with {
    ID
    @UI.Hidden;

    unit
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>UNIT}'
    @Common.FieldControl : #Mandatory;

    unit_code
    @UI.ExcludeFromNavigationContext
    @Common.Label        : '{i18n>UNIT}';

    numerator
    @Common.Label        : '{i18n>NUMERATOR}'
    @Common.FieldControl : #Mandatory;

    denominator
    @Common.Label        : '{i18n>DENOMINATOR}'
    @Common.FieldControl : #Mandatory;

    defaultUnit
    @Common.Label        : '{i18n>DEFAULT}';

    serviceMaterial
    @UI.Hidden;
}
