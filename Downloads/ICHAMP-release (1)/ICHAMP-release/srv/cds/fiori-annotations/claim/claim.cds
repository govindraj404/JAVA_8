using ClaimService from '../../services/index';

annotate ClaimService.Claims with {
     ID
     @UI.Hidden;

     identifier
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>CLAIM_NUMBER}'
     @Common.FieldControl : #ReadOnly;

     claimType
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>CLAIM_TYPE}'
     @Common.FieldControl : #ReadOnly;

     supplierRole
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>SUPPLIER_ROLE}'
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

     plant
     @Search.defaultSearchElement
     @Common.Label         : '{i18n>PLANT}'
          @Common               : {
          Text            : plant.plant,
          TextArrangement : #TextOnly
     };

     versionCategory
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>VERSION_CATEGORY}'
     @Common.FieldControl : #ReadOnly;
          
     quantity
     @Measures.Unit : unit
     @UI.HiddenFilter
     @Common.Label         : '{i18n>QUANTITY}';

          
     unit
     @Search.defaultSearchElement
     @Common.Label         : '{i18n>UNIT}';

     complaint
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>COMPLAINT}';

     status
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>STATUS}'
     @Common              : {
          Text            : status.name,
          TextArrangement : #TextOnly
     };

     itemType
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>ITEM_TYPE}'
     @Common.FieldControl : #ReadOnly;

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
     @Common.FieldControl : isClaimFieldControl
     @Common       :            {
          Text            : personResponsible.businessPartnerName1,
          TextArrangement : #TextOnly
     };

     requestedAmount
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>REQUESTED_AMOUNT}'
     @Common.FieldControl : #ReadOnly;

     paidAmount
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>PAID_AMOUNT}'
     @Common.FieldControl : #ReadOnly;

     decision
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>DECISION}'
     @Common.FieldControl : #ReadOnly;

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
     @Common.Label :    '{i18n>SUPPLIER_CONTACT_PERSON}'
     @Common       :    {
          Text            : contactPerson.businessPartnerName1,
          TextArrangement : #TextOnly
     };

     requestSentDate
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>REQUEST_SENT_DATE}'
     @Common.FieldControl : #ReadOnly;

     responseReceivedDate
     @Search.defaultSearchElement
     @Common.Label        : '{i18n>REQUEST_RECEIVED_DATE}'
     @Common.FieldControl : #ReadOnly;

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