using ReturnPurchaseOrderService from '../../services/index';

annotate ReturnPurchaseOrderService.ReturnPurchaseOrders with @(
    odata.draft.enabled           : true,
    Capabilities                  : {
        Updatable : true,
        Deletable : false,
    },
    UI  : {
        UpdateHidden : isUpdateRestricted,
        HeaderInfo                  : {
            TypeName       : '{i18n>RETURN_PURCHASE_ORDER}',
            TypeNamePlural : '{i18n>RETURN_PURCHASE_ORDERS}',
            Title          : {
                $Type : 'UI.DataField',
                Value : identifier
            },
             Description : {
                $Type : 'UI.DataField',
                Value : returnPurchaseType
            },
        },
        HeaderFacets : [
            {
                $Type  : 'UI.ReferenceFacet',
                Label  : '{i18n>PARTNER_AND_MATERIAL_DATA}',
                Target : '@UI.FieldGroup#HeaderDetails',
            },
            {
                $Type  : 'UI.ReferenceFacet',
                Label  : '{i18n>ORGANIZATION_DATA}',
                Target : '@UI.FieldGroup#HeaderFacetDetails',
            },
            {
                $Type  : 'UI.ReferenceFacet',
                Target : '@UI.DataPoint#Status',
            }
        ],
        Facets  : [
            {
                $Type  : 'UI.CollectionFacet',
                ID     : 'ReferenceInformationFacet',
                Label  : '{i18n>GENERAL_INFORMATION}',
                Facets : [
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionPODetails',
                        Label  : '{i18n>RETURN_PURCHASE_ORDER_INFORMATION}',
                        Target : '@UI.FieldGroup#ReturnPODetails'
                    },
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionInternalDetails',
                        Label  : '{i18n>INTERNAL_INFORMATION}',
                        Target : '@UI.FieldGroup#InternalDetails'
                    }
                ]
            }
        ],
        FieldGroup#HeaderDetails : {
            $Type : 'UI.FieldGroupType',
            Data  : [
                {
                    $Type : 'UI.DataField',
                    Value : supplier_ID,
                    Label : '{i18n>SUPPLIER}'
                },
                {
                    $Type : 'UI.DataField',
                    Value : material_ID,
                    Label : '{i18n>MATERIAL}',
                },
                {
                    $Type : 'UI.DataField',
                    Value : quantity
                }
            ]
        },
        FieldGroup#HeaderFacetDetails   : {
            $Type : 'UI.FieldGroupType',
            Data  : [
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
            ]
        },
        FieldGroup #ReturnPODetails : {
            Data : [
                {
                    $Type : 'UI.DataField',
                    Value : returnPurchaseType
                },
                {
                    $Type : 'UI.DataField',
                    Value : reason_code
                },
                {
                    $Type : 'UI.DataField',
                    Value : itemNumber
                },
                {
                    $Type : 'UI.DataField',
                    Value : purchasingGroup_code
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
        FieldGroup #InternalDetails : {
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
    // Common.SideEffects #returnPoPage : {
    //     SourceProperties : [
    //         complaint_ID,
    //         reason_code
    //     ],
    //     TargetProperties : [
    //         'supplier',
    //         'material',
    //         'plant',
    //         'purchasingOrganization',
    //         'quantity',
    //         'unit',
    //         'returnPurchaseType',
    //         'itemNumber',
    //         'company'  
    //     ],
    //     TargetEntities   : [
    //         BusinessPartner,
    //         Material,
    //         Plants,
    //         PurchaseOrganization
    //     ]
    // }
);