using ComplaintService from '../../services/index';

annotate ComplaintService.Complaints with {
ID
@UI.Hidden;

identifier
@Search.defaultSearchElement
@Common.FieldControl  : #ReadOnly
  @Common.Label         : '{i18n>COMPLAINT_NUMBER}';

  complaintStatus
  @Search.defaultSearchElement
  @Common.Label         : '{i18n>STATUS}'
  @Common               : {
    Text            : complaintStatus.name,
    TextArrangement : #TextOnly
  };

  description
  @UI.ExcludeFromNavigationContext
  @UI.MultiLineText
  @Common.Label         : '{i18n>DESCRIPTION}';


  referenceNumber
  @Search.defaultSearchElement
  @Common.Label         : '{i18n>REFERENCE_NUMBER}'
  @Common.FieldControl  : isComplaintNew;

  companyCode_ID
  @Common.FieldControl     : #ReadOnly
  @Common.Label         : '{i18n>COMPANY_CODE}';

  note
  @UI.MultiLineText
  @Common.Label         : '{i18n>NOTE}';


  personResponsible_ID
    @Common.ValueListMapping : {
        Label          : '{i18n>RESPONSIBLE_PERSON}',
        CollectionPath : 'BTPUsers',
        Parameters     : [
        {
        $Type             : 'Common.ValueListParameterInOut',
        ValueListProperty : 'personResponsible_ID',
        LocalDataProperty : personResponsible_ID
      },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'personResponsibleNumber'
        }
        ]
    }
     @Common.Label :            '{i18n>RESPONSIBLE_PERSON}';
 
  contactPerson
  @Common.ValueListMapping : {
    Label          : '{i18n>SUPPLIER_CONTACT_PERSON}',
    CollectionPath : 'BusinessPartners',
    Parameters     : [
      {
        $Type             : 'Common.ValueListParameterInOut',
        ValueListProperty : 'ID',
        LocalDataProperty : contactPerson_ID,
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

  creationType
  @Common.Label         : '{i18n>CREATION_TYPE}'
  @Common.FieldControl  : #ReadOnly;

  createdAt
  @Search.defaultSearchElement
  @UI.HiddenFilter      : false;

  createdBy
  @UI.HiddenFilter      : false;

  modifiedAt
  @UI.HiddenFilter      : false;

  modifiedBy
  @UI.HiddenFilter      : false;

  businessObjects
  @Search.defaultSearchElement
  @Common.Label         : '{i18n>BUSINESS_OBJECT_NUMBER}'
  @Common       :            {
    Text            : businessObjects.businessObjectID_ID,
    TextArrangement : #TextOnly
  };

  unit_code
  @UI.ExcludeFromNavigationContext
  @Common.Label         : '{i18n>UNIT}'
  @Common.FieldControl  : #ReadOnly;

  quantity
  @UI.ExcludeFromNavigationContext
  @Measures.Unit : unit_code
  @Common.FieldControl  : isComplaintNew
  @Common.Label         : '{i18n>QUANTITY}';

  purchasingOrganization
  @Common.Label         : '{i18n>PURCHASING_ORGANIZATION}'
  @Common.FieldControl  : isComplaintNew
  @Common               : {
    Text            : purchasingOrganization.purchaseOrganizationName,
    TextArrangement : #TextOnly
  };

  plant
  @Search.defaultSearchElement
  @Common.Label         : '{i18n>PLANT}'
  @Common.FieldControl :  isFieldControlMandatory
  @Common               : {
    Text            : plant.plantName,
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
        LocalDataProperty : supplier_ID,
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
  @Common.FieldControl  : isComplaintNew
  @Common.Label         : '{i18n>SUPPLIER}'
  @Common :               {
    Text            : supplier.businessPartnerName1,
    TextArrangement : #TextOnly
  };

  material
  @Search.defaultSearchElement
  @Common.FieldControl  : isComplaintNew
  @Common.Label         : '{i18n>MATERIAL}'
  @Common               : {
    Text            : material.materialCode,
    TextArrangement : #TextOnly
  };

  totalLaborHour
  @UI.ExcludeFromNavigationContext
  @Common.Label         : '{i18n>TOTAL_LABOR}'
  @Measures.Unit        : laborUnit_code;

  currency_code
  @UI.ExcludeFromNavigationContext
  @Common.Label         : '{i18n>CURRENCY}';

  laborUnit_code
  @UI.ExcludeFromNavigationContext
  @Common.Label         : '{i18n>UNIT}'
  @(Common : {ValueListMapping : {
    Label          : '{i18n>UNIT}',
    CollectionPath : 'UnitOfMeasure',
    Parameters     : [
    {
        $Type             : 'Common.ValueListParameterInOut',
        ValueListProperty : 'code',
        LocalDataProperty : laborUnit_code
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'name'
    }
    ]}})
  @Common.FieldControl  : #ReadOnly;

  totalSubLetCost
  @UI.ExcludeFromNavigationContext
  @Common.Label         : '{i18n>TOTAL_SUBLET_COST}'
  @Measures.ISOCurrency : currency_code;

  companyCode  
  @Common.FieldControl     : #ReadOnly
  @Common.Label         : '{i18n>COMPANY_CODE}'
  @Common : {
    Text            : companyCode.companyCodeName,
    TextArrangement : #TextOnly
  };

  isUpdateRestricted
  @UI.Hidden;

  isComplaintNew
  @UI.Hidden;

  isHideCostCollection
  @UI.Hidden;

  isFieldControlMandatory
  @UI.Hidden;

  isHideAdaptStreams
  @UI.Hidden;

  isShowStreams
  @UI.Hidden;

  isEventSpecificRequest
  @UI.Hidden;

  isHideDiscardComplaint
  @UI.Hidden;
  
  isHideReopenComplaint
  @UI.Hidden;
  
  isHideCloseComplaint
  @UI.Hidden;

  complaintType
  @Common.Label            : '{i18n>COMPLAINT_TYPE}';

  unit
  @Common.Label         : '{i18n>UNIT}';

  laborUnit
  @UI.Hidden;
}