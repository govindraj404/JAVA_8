using ConfigurationService from '../../services/index';

annotate ConfigurationService.DestinationConfigurations with @(UI : {
    HeaderInfo                  : {
        TypeName       : '{i18n>DESTINATION}',
        TypeNamePlural : '{i18n>DESTINATIONS}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>DESTINATION}',
            Value : destination
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>BUSINESS_OBJECT_TYPE}',
            Value : businessObjectType_code
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
            $Type                   : 'UI.DataField',
            Value                   : companyCode_ID,
            ![@Common.FieldControl] : #Mandatory
        },
        {
            $Type : 'UI.DataField',
            Value : businessObjectType_code
        },
        {
            $Type : 'UI.DataField',
            Value : description
        },
        {
            $Type : 'UI.DataField',
            Value : navigationDestination
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
    FieldGroup #Other           : {Data : [
        {
            $Type : 'UI.DataField',
            Value : identifier
        },
        {
            $Type : 'UI.DataField',
            Value : businessObjectType_code
        },
        {
            $Type : 'UI.DataField',
            Value : navigationDestination
        }
    ]}
});
