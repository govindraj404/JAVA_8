using QualityNotificationService from '../../services/index';

annotate QualityNotificationService.QualityNotifications with @(
    odata.draft.enabled             : true,
    Capabilities                    : {
        Updatable : true,
        Deletable : false,
    },
    UI                              : {
        UpdateHidden                   : isUpdateRestricted,
        HeaderInfo                     : {
            TypeName       : '{i18n>QUALITY_NOTIFICATION}',
            TypeNamePlural : '{i18n>QUALITY_NOTIFICATION}',
            Title          : {
                $Type : 'UI.DataField',
                Value : identifier
            },
            Description    : {
                $Type : 'UI.DataField',
                Value : qnType
            }
        },
        HeaderFacets                   : [
            {
                $Type  : 'UI.ReferenceFacet',
                Label  : '{i18n>PARTNER_AND_MATERIAL_DATA}',
                Target : '@UI.FieldGroup#HeaderDetails'
            },
            {
                $Type  : 'UI.ReferenceFacet',
                Label  : '{i18n>ORGANIZATION_DATA}',
                Target : '@UI.FieldGroup#HeaderFacetDetails',
            },
            {
                $Type  : 'UI.ReferenceFacet',
                Target : '@UI.DataPoint#Status'
            }
        ],
        Facets                         : [
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>GENERAL_INFORMATION}',
                ID     : 'ReferenceInformationFacet',
                Facets : [
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionQualityDetails',
                        Label  : '{i18n>QUALITY_NOTIFICATION_INFORMATION}',
                        Target : '@UI.FieldGroup#QualityDetails'
                    },
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionInternalDetails',
                        Label  : '{i18n>INTERNAL_INFORMATION}',
                        Target : '@UI.FieldGroup#InternalDetails'
                    }
                ]
            },
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>DEFECT}',
                ID     : 'DefectFacet',
                Facets : [{
                    $Type  : 'UI.ReferenceFacet',
                    ID     : 'SubSectionDefectDetails',
                    Label  : '{i18n>DEFECT}',
                    Target : '@UI.FieldGroup#Defect'
                }, ]
            }
        ],
        FieldGroup #QualityDetails     : {Data : [
            {
                $Type : 'UI.DataField',
                Value : qnType
            },
            {
                $Type : 'UI.DataField',
                Value : purchaseOrderNumber
            },
            {
                $Type : 'UI.DataField',
                Value : purchaseOrderItem
            },
            {
                $Type : 'UI.DataField',
                Value : personResponsible_ID
            },
            {
                $Type : 'UI.DataField',
                Value : contactPerson_ID
            }
        ]},
        FieldGroup #Defect             : {Data : [
            {
                $Type : 'UI.DataField',
                Value : defect.identifier
            },
            {
                $Type : 'UI.DataField',
                Value : defect.defectGroup_code
            },
            {
                $Type : 'UI.DataField',
                Value : defect.defectCode_code
            },
            {
                $Type : 'UI.DataField',
                Value : defect.description
            }
        ]},
        FieldGroup #InternalDetails    : {Data : [
            {
                $Type : 'UI.DataField',
                Value : createdBy
            },
            {
                $Type : 'UI.DataField',
                Value : createdAt
            },
            {
                $Type : 'UI.DataField',
                Value : modifiedBy
            },
            {
                $Type : 'UI.DataField',
                Value : modifiedAt
            }
        ]},
        FieldGroup #HeaderDetails      : {Data : [
            {
                $Type : 'UI.DataField',
                Value : supplier_ID,
                Label : '{i18n>SUPPLIER}'
            },
            {
                $Type : 'UI.DataField',
                Value : material_ID,
                Label : '{i18n>MATERIAL}'
            },
            {
                $Type : 'UI.DataField',
                Value : quantity
            }
        ]},
        FieldGroup #HeaderFacetDetails : {Data : [
            {
                $Type : 'UI.DataField',
                Value : plant_ID,
                Label : '{i18n>PLANT}'
            },
            {
                $Type : 'UI.DataField',
                Value : company_ID
            },
            {
                $Type : 'UI.DataField',
                Value : purchasingOrganization_ID,
                Label : '{i18n>PURCHASING_ORGANIZATION}'
            }
        ]},
        DataPoint #Status              : {
            Value : status_code,
            Title : '{i18n>STATUS}'
        },
    },
    Common.SideEffects #defectCodes : {
        SourceProperties : [defect.defectCode_code],
        TargetProperties : ['defect.defectGroup_code'],
        TargetEntities   : [
            defect.defectCode,
            defect.defectGroup
        ]
    }
);
