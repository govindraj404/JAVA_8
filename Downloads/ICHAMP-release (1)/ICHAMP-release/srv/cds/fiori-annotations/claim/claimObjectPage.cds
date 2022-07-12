using ClaimService from '../../services/index';

annotate ClaimService.Claims with @(
    odata.draft.enabled           : true,
    Capabilities                  : {
        Updatable : true,
        Deletable : false,
    },
    UI                             : {
        UpdateHidden : isUpdateRestricted,
        HeaderInfo   : {
            TypeName       : '{i18n>CLAIM}',
            TypeNamePlural : '{i18n>CLAIMS}',
            Title          : {
                $Type : 'UI.DataField',
                Value : identifier
            },
            Description    : {
                $Type : 'UI.DataField',
                Value : claimType
            }
        },
        HeaderFacets : [
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
        Facets: [
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>GENERAL_INFORMATION}',
                ID     : 'sGeneralMainCollectionFacets',
                Facets : [{
                    $Type  : 'UI.CollectionFacet',
                    Label  : '{i18n>GENERAL_INFORMATION}',
                    ID     : 'sGeneralCollectionFacets',
                    Facets : [
                        {
                            $Type  : 'UI.ReferenceFacet',
                            ID     : 'SubSectionClaimDetails',
                            Label  : '{i18n>CLAIM_DETAILS}',
                            Target : '@UI.FieldGroup#ClaimDetails'
                        },
                        {
                            $Type  : 'UI.ReferenceFacet',
                            ID     : 'SubSectionInternalDetails',
                            Label  : '{i18n>INTERNAL_INFORMATION}',
                            Target : '@UI.FieldGroup#InternalDetails'
                        }
                    ]
                }]
            },
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>COST}',
                ID     : 'sCostCollectorDetail',
                Facets : [
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'sCostCollectorInformation',
                        Label  : '{i18n>ITEMS}',
                        Target : 'costCollectors/@UI.LineItem'
                    }
                ]
            }
        ],
        FieldGroup#HeaderDetails : {
            $Type : 'UI.FieldGroupType',
            Data  : [
                {
                    $Type : 'UI.DataField',
                    Value : supplier_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : material_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : quantity
                }
            ]
        },
        FieldGroup#HeaderFacetDetails : {
            $Type : 'UI.FieldGroupType',
            Data  : [
                {
                    $Type : 'UI.DataField',
                    Value : plant_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : company_ID
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>PURCHASING_ORGANIZATION}',
                    Value : purchasingOrganization_ID
                }
            ]
        },
        FieldGroup #ClaimDetails  : {
            Data : [
                {
                    $Type : 'UI.DataField',
                    Value : itemType_code
                },
                {
                    $Type : 'UI.DataField',
                    Value : claimType
                },
                {
                    $Type : 'UI.DataField',
                    Value : supplierRole
                },
                {
                    $Type : 'UI.DataField',
                    Value : versionCategory
                },
                {
                    $Type : 'UI.DataField',
                    Value : personResponsible_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : contactPerson_ID
                }
            ]
        },
        FieldGroup #InternalDetails    : {
            Data : [
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
            ]
        },
        DataPoint #Status  : {
            Value : status_code,
            Title : '{i18n>STATUS}'
        }
    }
    // Common.SideEffects #claimPage : {
    //     SourceProperties : [
    //         complaint_ID,
    //         personResponsible_ID
    //     ],
    //     TargetProperties : [
    //         'supplier',
    //         'material',
    //         'plant',
    //         'purchasingOrganization',
    //         'supplierRole',
    //         'versionCategory',
    //         'claimType',
    //         'itemType_code',
    //         'quantity',
    //         'unit',
    //         'company',
    //         'costCollectors'
    //     ],
    //     TargetEntities   : [
    //         BusinessPartner,
    //         Material,
    //         Plants,
    //         PurchaseOrganization,
    //         CostCollectors
    //     ]
    // }
);
