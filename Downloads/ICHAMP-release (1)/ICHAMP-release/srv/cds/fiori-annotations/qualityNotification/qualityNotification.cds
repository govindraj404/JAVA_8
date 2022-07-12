using QualityNotificationService from '../../services/index';

annotate QualityNotificationService.QualityNotifications with {

    ID
    @UI.Hidden;

    identifier
    @Common.Label        : '{i18n>QUALITY_NOTIFICATION_NUMBER}'
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

    qnType
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>TYPE}'
    @Common.FieldControl : #ReadOnly;


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

    unit
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>UNIT}';

    quantity
    @Measures.Unit : unit
    @UI.HiddenFilter
    @Common.Label         : '{i18n>QUANTITY}';

    plant
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>PLANT}'
    @Common               : {
        Text            : plant.plant,
        TextArrangement : #TextOnly
    };

    purchasingOrganization
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>PURCHASING_ORGANIZATION}'
    @Common               : {
        Text            : purchasingOrganization.purchaseOrganizationName,
        TextArrangement : #TextOnly
    };
    
    company
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>COMPANY}'
    @Common              : {
        Text            : company.companyCodeName,
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
    @Common.FieldControl : isQualityNotificationFieldControlMandatory
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

    inspectionResult
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>INSPECTION_RESULT}'
    @UI.Hidden;

    purchaseOrderNumber
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>PURCHASE_ORDER_NUMBER}'
    @Common.FieldControl : isQualityNotificationFieldControl;
    
    purchaseOrderItem
    @Search.defaultSearchElement
    @Common.Label        : '{i18n>PURCHASE_ORDER_ITEM}'
    @Common.FieldControl : isQualityNotificationFieldControl;

    @Core.Computed: false
    sNavigation;
}