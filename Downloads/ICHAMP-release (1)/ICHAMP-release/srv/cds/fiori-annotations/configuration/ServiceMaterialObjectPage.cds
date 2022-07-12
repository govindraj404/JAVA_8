using ConfigurationService from '../../services/index';

annotate ConfigurationService.ServiceMaterials with @(
    UI                              : {
        HeaderInfo                  : {
            TypeName       : '{i18n>SERVICE_MATERIAL}',
            TypeNamePlural : '{i18n>SERVICE_MATERIALS}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>MATERIAL}',
                Value : serviceMaterial
            },
            Description    : {
                $Type : 'UI.DataField',
                Label : '{i18n>SUB_ITEM_TYPE}',
                Value : subItemType_code
            },
        },
        Facets                      : [
            {
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
            },
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>UNITS}',
                ID     : 'ReferenceInformationFacet',
                Facets : [{
                    $Type  : 'UI.ReferenceFacet',
                    ID     : 'serviceMaterialUnit',
                    Label  : '{i18n>ITEMS}',
                    Target : 'serviceMaterialUnit/@UI.LineItem'
                }]
            }
        ],
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
                Value : subItemType_code
            },
            {
                $Type : 'UI.DataField',
                Value : itemType_code
            },
            {
                $Type : 'UI.DataField',
                Value : serviceMaterial
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
            Value : identifier
        }]}
    },
    Common.SideEffects #subItemType : {
        SourceProperties : [subItemType_code],
        TargetProperties : ['itemType_code']
    }
);
