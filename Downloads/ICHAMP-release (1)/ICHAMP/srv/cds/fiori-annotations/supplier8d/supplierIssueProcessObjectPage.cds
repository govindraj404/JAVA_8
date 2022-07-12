using SupplierIssueProcessService from '../../services/index';

annotate SupplierIssueProcessService.Supplier8DProcesses with @(
    odata.draft.enabled : true,
    Capabilities        : {
        Updatable : true,
        Deletable : false,
    },
    UI   : {
        UpdateHidden : isUpdateRestricted,
        HeaderInfo                     : {
            TypeName       : '{i18n>8D_REPORT}',
            TypeNamePlural : '{i18n>8D_REPORTS}',
            Title          : {
                $Type : 'UI.DataField',
                Value : identifier
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
                Target : '@UI.FieldGroup#HeaderFacetDetails'
            },
            {
                $Type  : 'UI.ReferenceFacet',
                Target : '@UI.DataPoint#Status'
            }
        ],
        Facets   : [
            {
                $Type  : 'UI.CollectionFacet',
                Label  : '{i18n>GENERAL_INFORMATION}',
                ID     : 'ReferenceInformationFacet',
                Facets : [
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionReportDetails',
                        Label  : '{i18n>REPORT_INFORMATION}',
                        Target : '@UI.FieldGroup#GeneralDetails'
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
                Facets : [
                    {
                        $Type  : 'UI.ReferenceFacet',
                        ID     : 'SubSectionDefectDetails',
                        Label  : '{i18n>DEFECT}',
                        Target : '@UI.FieldGroup#DefectDetails'
                    }
                ]
            }
        ],
        FieldGroup #HeaderDetails  : {
            $Type : 'UI.FieldGroupType',
            Data  : [
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>SUPPLIER}',
                    Value : supplier_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.parent.identifier
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>MATERIAL}',
                    Value : material_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : quantity
                }
            ]
        },
        FieldGroup #HeaderFacetDetails : {
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
        FieldGroup #GeneralDetails     : {
            Data : [
                {
                    $Type : 'UI.DataField',
                    Value : supplierIssueProcessesType
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.parent.purchaseOrderNumber
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.parent.purchaseOrderItem
                },
                {
                    $Type : 'UI.DataField',
                    Value : personResponsible_ID
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>SUPPLIER_CONTACT_PERSON}',
                    Value : contactPerson_ID
                },
                {
                    $Type : 'UI.DataField',
                    Value : requestStartDate
                },
                {
                    $Type : 'UI.DataField',
                    Value : requestEndDate
                },
                {
                    $Type : 'UI.DataField',
                    Value : actualStartDate
                },
                {
                    $Type : 'UI.DataField',
                    Value : actualEndDate
                }
            ]
        },
        FieldGroup #DefectDetails     : {
            Data : [
                {
                    $Type : 'UI.DataField',
                    Value : defect.identifier,
                    Label : '{i18n>NUMBER}'
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.defectGroup_code,
                    Label : '{i18n>GROUP}'
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.defectCode_code,
                    Label : '{i18n>CODE}'
                },
                {
                    $Type : 'UI.DataField',
                    Value : defect.description,
                    Label : '{i18n>DESCRIPTION}'
                }
            ]
        },
        FieldGroup #InternalDetails   : {
            Data : [
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>CREATED_BY}',
                    Value : createdBy
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>CREATED_ON}',
                    Value : createdAt
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>CHANGED_BY}',
                    Value : modifiedBy
                },
                {
                    $Type : 'UI.DataField',
                    Label : '{i18n>CHANGED_ON}',
                    Value : modifiedAt
                }
            ]
        },
        DataPoint #Status : {
            Value : status_code,
            Title : '{i18n>STATUS}'
        }
    }
    // Common.SideEffects #supplier8dPage : {
    //     SourceProperties : [complaint_ID,personResponsible_ID],
    //     TargetProperties : [
    //         'supplier',
    //         'material',
    //         'plant',
    //         'purchasingOrganization',
    //         'quantity',
    //         'unit',
    //         'supplierIssueProcessesType',
    //         'defect.parent.identifier',
    //         'defect.parent',
    //         'defect.defectGroup',
    //         'defect.defectCode',
    //         'defect',
    //         'company'
    //     ],
    //     TargetEntities   : [
    //         BusinessPartner,
    //         Material,
    //         Plants,
    //         PurchaseOrganization,
    //         Defects
    //     ]
    // }
);