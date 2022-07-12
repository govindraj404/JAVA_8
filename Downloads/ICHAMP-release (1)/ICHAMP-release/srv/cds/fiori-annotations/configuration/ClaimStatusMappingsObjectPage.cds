using ConfigurationService from '../../services/index';

annotate ConfigurationService.ClaimStatusMappings with @(UI : {
    HeaderInfo                  : {
        TypeName       : '{i18n>CLAIM_STATUS}',
        TypeNamePlural : '{i18n>CLAIM_STATUS}',
        Title          : {
            $Type : 'UI.DataField',
            Value : code,
            Label : '{i18n>BACKEND_STATUS_CODE}'
        },
        Description    : {
            $Type : 'UI.DataField',
            Value : status_code,
            Label : '{i18n>COCKPIT_STATUS}'
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
                ID     : 'SubSectionInternalDetails',
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
            Value : code
        },
        {
            $Type : 'UI.DataField',
            Value : name
        },
        {
            $Type : 'UI.DataField',
            Value : status_code
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
        Value : identifier
    }]}
});
