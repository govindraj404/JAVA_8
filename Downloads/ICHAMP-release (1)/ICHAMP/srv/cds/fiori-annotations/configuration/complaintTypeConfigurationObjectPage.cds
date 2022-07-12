using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeConfigurations with @(
    UI                                          : {
        Identification :[
            {Value : ID},
             {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>ACTIVATE}',
                Action: 'ConfigurationService.ReactivateComplaintTypeConfigurations',
                ![@UI.Hidden]: isActive
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>DEACTIVATE}',
                Action: 'ConfigurationService.DeactivateComplaintTypeConfigurations',
                ![@UI.Hidden]: isInActive
            }
        ],
        HeaderInfo                  : {
            TypeName       : '{i18n>COMPLAINT_TYPE}',
            TypeNamePlural : '{i18n>COMPLAINT_TYPES}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>CODE}',
                Value : code
            },
            Description    : {
                $Type : 'UI.DataField',
                Label : '{i18n>DESCRIPTION}',
                Value : description
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
                Label  : '{i18n>SALES_AREA_MAPPINGS}',
                ID     : 'ComplaintTypeToSalesAreaMappingsFacetFacet',
                Facets : [{
                    $Type  : 'UI.ReferenceFacet',
                    ID     : 'complaintTypeToSalesAreaMappings',
                    Label  : '{i18n>ITEMS}',
                    Target : 'complaintTypeToSalesAreaMappings/@UI.LineItem'
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
                Value : code
            },
            {
                $Type : 'UI.DataField',
                Value : description
            },
            {
                $Type : 'UI.DataField',
                Value : individualComplaintType
            },
            {
                $Type : 'UI.DataField',
                Value : itemCategory_ID
            },
            {
                $Type : 'UI.DataField',
                Value : complaintCategory_code
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
    Common.SideEffects #itemCategory            : {
        SourceProperties : [itemCategory_ID],
        TargetEntities   : [itemCategory]
    },
    Common.SideEffects #individualComplaintType : {
        SourceProperties : [individualComplaintType],
        TargetProperties : [
            'isFieldControlIndividualComplaintType',
            'itemCategory_ID',
            'individualComplaintType'
        ]
    },
     Common.SideEffects #itemCategoryChangeToCompalintcategory : {
        SourceProperties : [itemCategory_ID],
        TargetProperties : ['complaintCategory_code']
    }
);
