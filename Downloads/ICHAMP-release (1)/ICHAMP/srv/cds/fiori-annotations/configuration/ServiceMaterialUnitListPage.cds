using ConfigurationService from '../../services/index';

annotate ConfigurationService.ServiceMaterialUnits with @(

    Capabilities : {SearchRestrictions.Searchable : false, },
    UI           : {
        HeaderInfo : {
            TypeName       : '{i18n>ITEM}',
            TypeNamePlural : '{i18n>ITEMS}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>SERVICE_MATERIAL}',
                Value : serviceMaterial_ID
            },
            Description    : {
                $Type : 'UI.DataField',
                Label : '{i18n>UNIT}',
                Value : unit_code
            },
        },
        LineItem   : {$value : [
            {
                $Type                 : 'UI.DataField',
                Value                 : unit_code,
                ![@UI.Importance]     : #High,
                ![@HTML5.CssDefaults] : {width : '8rem'}
            },
            {
                $Type             : 'UI.DataField',
                Value             : numerator,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : denominator,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : defaultUnit,
                ![@UI.Importance] : #High
            }
        ]}
    }
);
