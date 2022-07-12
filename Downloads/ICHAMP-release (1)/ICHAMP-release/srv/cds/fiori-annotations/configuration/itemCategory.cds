using ConfigurationService from '../../services/index';

annotate ConfigurationService.ItemCategories {

    ID
    @UI.Hidden
    @Common       : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : code,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    isFieldControlEnableMaterial
    @UI.Hidden;

    isFieldControlEnableConditionType
    @UI.Hidden;

    identifier
    @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    code
    @Common       : {
        Label        : '{i18n>ITEM_CATEGORY}',
        FieldControl : #Mandatory
    };

    description
    @Common       : {Label : '{i18n>DESCRIPTION}'};

    complaintCategory
    @Common       : {
        Text             : complaintCategory.name,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>COMPLAINT_CATEGORY}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>COMPLAINT_CATEGORY}',
            CollectionPath : 'ComplaintCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : complaintCategory_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'name'
                }
            ]
        }
    };

    complaintCategory_code
    @Common.Label : '{i18n>COMPLAINT_CATEGORY}';

    complaintQuantityRule
    @Common       : {
        Label           : '{i18n>COMPLAINT_QUANTITY_RULE}',
        Text            : complaintQuantityRule.name,
        TextArrangement : #TextOnly,
        FieldControl    : #Mandatory
    };

    complaintQuantityRule_code
    @Common       : {Label : '{i18n>COMPLAINT_QUANTITY_RULE}'};

    materialEnteredManually
    @Common       : {Label : '{i18n>MATERIAL_ENTERED_MANUALLY}'};

    receivedQuantityEditable
    @Common       : {Label : '{i18n>RECEIVED_QUANTITY_EDITABLE}'};

    returnQuantityEditable
    @UI.Hidden
    @Common       : {Label : '{i18n>RETURN_QUANTITY_EDITABLE}'};

    individualComplaint
    @Common       : {Label : '{i18n>INDIVIDUAL_COMPLAINT}'};

    externalReferenceMandatory
    @Common       : {Label : '{i18n>EXTERNAL_REFERENCE_REQUIRED}'};

    externalReferenceCheckedForDuplication
    @UI.Hidden
    @Common       : {Label : '{i18n>EXTERNAL_REFERENCE_CHECKED_FOR_DUPLICATION}'};

    referenceDocumentCategory
    @Common       : {
        Label           : '{i18n>REFERENCE_CATEGORY}',
        Text            : referenceDocumentCategory.description,
        TextArrangement : #TextOnly
    };

    referenceDocumentCategory_code
    @Common.Label : '{i18n>REFERENCE_CATEGORY}';

    material
    @Common       : {
        FieldControl     : isFieldControlEnableMaterial,
        Label            : '{i18n>MATERIAL}',
        Text             : material.materialDescription,
        TextArrangement  : #TextOnly,
        ValueListMapping : {
            Label          : '{i18n>MATERIAL}',
            CollectionPath : 'MaterialMasterGeneralDatas',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'ID',
                    LocalDataProperty : material_ID
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'materialCode',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'materialDescription',
                    ![@UI.Importance] : #High
                }
            ]
        }
    };

    material_ID
    @Common.Label : '{i18n>MATERIAL}';

    relevantForLogisticProcess
    @Common.Label : '{i18n>RELEVANT_FOR_LOGISTICAL_PROCESS}';

    creditDebitAmountEnteredManually
    @Common       : {Label : '{i18n>CREDIT_DEBIT_AMOUNT_ENTERED_MANUALLY}'};

    conditionType
    @Common       : {
        Label        : '{i18n>CONDITION_TYPE}',
        FieldControl : isFieldControlEnableConditionType
    };

    isInActive
    @UI.Hidden;
    isActive
    @Common.Label : '{i18n>ACTIVE}';
}
