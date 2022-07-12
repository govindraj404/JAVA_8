using ConfigurationService from '../../services/index';

annotate ConfigurationService.BusinessObjectConfigurations with @(
    UI                                          : {
        HeaderInfo                  : {
            TypeName       : '{i18n>BUSINESS_OBJECT}',
            TypeNamePlural : '{i18n>BUSINESS_OBJECTS}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>BUSINESS_OBJECT_ATTRIBUTES}',
                Value : businessObjectAttribute_code
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
                $Type : 'UI.DataField',
                Value : complaintType_code
            },
            {
                $Type : 'UI.DataField',
                Value : businessObjectType_code
            },
            {
                $Type : 'UI.DataField',
                Value : businessObjectAttribute_code
            },
            {
                $Type : 'UI.DataField',
                Value : businessObjectValue
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
                Value : destination
            }
        ]}
    },
    Common.SideEffects #businessObjectAttribute : {
        SourceProperties : [businessObjectAttribute_code],
        TargetProperties : ['businessObjectType_code']
    }
);
