using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintCatItemCatMappings with @(UI : {
    HeaderInfo                  : {
        TypeName       : '{i18n>TARGET_REFERENCE_TYPE_MAPPING}',
        TypeNamePlural : '{i18n>TARGET_REFERENCE_TYPE_MAPPINGS}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>ITEM_CATEGORY}',
            Value : itemCategory.code
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>ITEM_CATEGORY_DESCRIPTION}',
            Value : itemCategory.description
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
            Label  : '{i18n>REFERENCE_TYPES}',
            ID     : 'ReferenceTypeFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'targetReferenceTypeMappings',
                Label  : '{i18n>ITEMS}',
                Target : 'targetReferenceTypeMappings/@UI.LineItem'
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
            Value : complaintCategory_code
        },
        {
            $Type : 'UI.DataField',
            Value : itemCategory_ID
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
