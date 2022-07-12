using ReturnPurchaseOrderService from '../../services/index';

annotate ReturnPurchaseOrderService.ReturnPurchaseOrders with {
    ID
    @UI.Hidden;

    identifier
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>RETURN_PURCHASE_ORDER_NUMBER}'
    @Common.FieldControl : #ReadOnly;

    complaint
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>COMPLAINT_ID}';

    status
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>STATUS}'
    @Common              : {
        Text            : status.name,
        TextArrangement : #TextOnly
    };

    returnPurchaseType
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>TYPE}'
    @Common.FieldControl : #ReadOnly;

    itemNumber
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>ITEM_NUMBER}'
    @Common.FieldControl : #ReadOnly;

    purchasingGroup
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>PURCHASING_GROUP}'
    @Common.FieldControl : isReturnOrderFieldControlMandatory
    @Common              : {
        Text            : purchasingGroup_code,
        TextArrangement : #TextOnly
    };

    supplier
    @Common.ValueListMapping : {
        Label          : '{i18n>SUPPLIER}',
        CollectionPath : 'BusinessPartners',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : supplier_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerNumber',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName1',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName2',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/houseNumber',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/address',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/city',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/postalCode',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/country',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/mobile',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/email',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'businessPartnerType',
                Constant : 'SUP'
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'isMarkedForDeletion',
                Constant : 'false'
            }
        ]
    }
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>SUPPLIER}'
    @Common               : {
        Text            : supplier.businessPartnerNumber,
        TextArrangement : #TextOnly
    };

    material
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>MATERIAL}'
    @Common               : {
        Text            : material.materialCode,
        TextArrangement : #TextOnly
    };

    reason
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>REASON}'
    @Common.FieldControl : isReturnOrderFieldControlMandatory
    @Common               : {
        Text            : reason.description,
        TextArrangement : #TextOnly
    };

    quantity
    @Measures.Unit : unit
    @UI.HiddenFilter
    @Common.Label         : '{i18n>QUANTITY}';

    unit
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>UNIT}';

    plant
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>PLANT}'
    @Common               : {
        Text            : plant.plant,
        TextArrangement : #TextOnly
    };

    personResponsible
    @Common.ValueListMapping : {
        Label          : '{i18n>RESPONSIBLE_PERSON}',
        CollectionPath : 'BusinessPartners',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : personResponsible_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerNumber',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName1',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName2',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/houseNumber',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/address',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/city',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/postalCode',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/country',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/mobile',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/email',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'businessPartnerType',
                Constant : 'PERRES'
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'isMarkedForDeletion',
                Constant : 'false'
            }
        ]
    }
    @Common.Label :            '{i18n>RESPONSIBLE_PERSON}'
    @Common.FieldControl : isReturnOrderFieldControl
    @Common       :            {
        Text            : personResponsible.businessPartnerName1,
        TextArrangement : #TextOnly
    };

    contactPerson
    @Common.ValueListMapping : {
        Label          : '{i18n>SUPPLIER_CONTACT_PERSON}',
        CollectionPath : 'BusinessPartners',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : contactPerson_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerNumber',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName1',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName2',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/houseNumber',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/address',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/city',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/postalCode',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/country',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/mobile',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/email',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'businessPartnerType',
                Constant : 'SUPCON'
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'isMarkedForDeletion',
                Constant : 'false'
            }
        ]
    }
    @Common.Label :            '{i18n>SUPPLIER_CONTACT_PERSON}'
    @Common       :            {
        Text            : contactPerson.businessPartnerName1,
        TextArrangement : #TextOnly
    };

    purchasingOrganization
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>PURCHASING_ORGANIZATION}'
    @Common               : {
        Text            : purchasingOrganization.purchaseOrganizationName,
        TextArrangement : #TextOnly
    };

    @Core.Computed: false
    sNavigation;

    company
    @Common.Label         : '{i18n>COMPANY}'
    @Common : {
        Text            : company.companyCodeName,
        TextArrangement : #TextOnly
    };
}