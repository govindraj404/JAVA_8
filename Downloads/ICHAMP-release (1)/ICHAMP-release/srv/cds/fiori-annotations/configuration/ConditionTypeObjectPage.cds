using ConfigurationService from '../../services/index';

annotate ConfigurationService.ConditionTypes with @(UI : {
    HeaderInfo                  : {
        TypeName       : '{i18n>CONDITION_TYPE}',
        TypeNamePlural : '{i18n>CONDITION_TYPES}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>CONDITION_TYPE}',
            Value : conditionType
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>ITEM_TYPE}',
            Value : itemType_code
        },
    },

    Facets                      : [{
        $Type  : 'UI.CollectionFacet',
        Label  : '{i18n>GENERAL_INFORMATION}',
        ID     : 'GeneralInformationFacet',
        Facets : [
            {
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionMainDetails',
                Label  : '{i18n>MAIN_DETAILS}',
                Target : '@UI.FieldGroup#MainDetails'
            },
            {
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionAdminData',
                Label  : '{i18n>INTERNAL_DETAILS}',
                Target : '@UI.FieldGroup#InternalDetails'
            }
        ]
    }],
    FilterFacets                : [{
        $Type  : 'UI.ReferenceFacet',
        ID     : 'FilterFacetOther',
        Label  : '{i18n>OTHER}',
        Target : '@UI.FieldGroup#Other'
    }],
    FieldGroup #MainDetails     : {Data : [
        {
            $Type : 'UI.DataField',
            Value : destination
        },
        {
            $Type : 'UI.DataField',
            Value : itemType_code
        },
        {
            $Type : 'UI.DataField',
            Value : conditionType
        },
        {
            $Type : 'UI.DataField',
            Value : description
        }
    ]},
    FieldGroup #InternalDetails : {Data : [
        {
            $Type : 'UI.DataField',
            Value : createdAt
        },
        {
            $Type : 'UI.DataField',
            Value : createdBy
        },
        {
            $Type : 'UI.DataField',
            Value : modifiedAt
        },
        {
            $Type : 'UI.DataField',
            Value : modifiedBy
        }
    ]},
    FieldGroup #Other           : {Data : [{
        $Type : 'UI.DataField',
        Value : description
    }]}
});
