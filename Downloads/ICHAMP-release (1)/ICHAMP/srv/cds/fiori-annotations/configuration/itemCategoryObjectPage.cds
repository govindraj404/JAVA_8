using ConfigurationService from '../../services/index';

annotate ConfigurationService.ItemCategories with @(
    UI                                                    : {
        Identification :[
        {Value : ID},
        {
            $Type  : 'UI.DataFieldForAction',
            Label  : '{i18n>ACTIVATE}',
            Action: 'ConfigurationService.ReactivateItemCategory',
            ![@UI.Hidden]: isActive
        },
        {
            $Type  : 'UI.DataFieldForAction',
            Label  : '{i18n>DEACTIVATE}',
            Action: 'ConfigurationService.DeactivateItemCategory',
            ![@UI.Hidden]: isInActive
        }
        ],
        HeaderInfo                  : {
            TypeName       : '{i18n>ITEM_CATEGORY}',
            TypeNamePlural : '{i18n>ITEM_CATEGORIES}',
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
                Value : complaintQuantityRule_code
            },
            {
                $Type : 'UI.DataField',
                Value : materialEnteredManually
            },
            {
                $Type : 'UI.DataField',
                Value : material_ID
            },
            {
                $Type : 'UI.DataField',
                Value : referenceDocumentCategory_code
            },
            {
                $Type : 'UI.DataField',
                Value : individualComplaint
            },
            {
                $Type : 'UI.DataField',
                Value : receivedQuantityEditable
            },
            {
                $Type : 'UI.DataField',
                Value : externalReferenceMandatory
            },
            {
                $Type : 'UI.DataField',
                Value : relevantForLogisticProcess
            },
            {
                $Type : 'UI.DataField',
                Value : creditDebitAmountEnteredManually
            },
            {
                $Type : 'UI.DataField',
                Value : conditionType
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
                Value : complaintCategory_code
            },
            {
                $Type : 'UI.DataField',
                Value : complaintQuantityRule_code
            },
            {
                $Type : 'UI.DataField',
                Value : externalReferenceMandatory
            },
            {
                $Type : 'UI.DataField',
                Value : externalReferenceCheckedForDuplication
            },
            {
                $Type : 'UI.DataField',
                Value : material_ID
            },
            {
                $Type : 'UI.DataField',
                Value : receivedQuantityEditable
            }
        ]}
    },
    Common.SideEffects #materialEnteredManually           : {
        SourceProperties : [materialEnteredManually],
        TargetProperties : [
            'isFieldControlEnableMaterial',
            'material_ID',
            'materialEnteredManually',
            'referenceDocumentCategory_code'
        ]
    },

    Common.SideEffects #referenceDocumentCategorymodified : {
        SourceProperties : [referenceDocumentCategory_code],
        TargetProperties : [
            'isFieldControlEnableMaterial',
            'material_ID',
            'referenceDocumentCategory_code',
            'materialEnteredManually'
        ]
    },
    Common.SideEffects #creditDebitAmountEnteredManually : {
        SourceProperties : [creditDebitAmountEnteredManually],
        TargetProperties : [
            'isFieldControlEnableConditionType',
            'creditDebitAmountEnteredManually',
            'conditionType'
        ]
    }
);
