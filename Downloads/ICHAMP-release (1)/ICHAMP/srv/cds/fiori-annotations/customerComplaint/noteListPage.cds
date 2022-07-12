using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.Notes with @(
    Capabilities : {SearchRestrictions.Searchable : false, },
    UI           : {
        HeaderInfo : {
            TypeName       : '{i18n>ITEM}',
            TypeNamePlural : '{i18n>ITEMS}'
        },
        LineItem   : {$value : [
            {
                $Type                 : 'UI.DataField',
                Value                 : noteType_code,
                ![@UI.Importance]     : #High,
                ![@HTML5.CssDefaults] : {width : '10rem'}
            },
            {
                $Type                 : 'UI.DataField',
                Value                 : note,
                ![@UI.Importance]     : #High,
                ![@HTML5.CssDefaults] : {width : '32rem'}
            },
            {
                $Type             : 'UI.DataField',
                Value             : createdAt,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : createdBy,
                ![@UI.Importance] : #High
            }
        ]}
    }
);
