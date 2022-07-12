using SupplierIssueProcessService from '../../services/index';

annotate SupplierIssueProcessService.Supplier8DProcesses with {
  ID
  @UI.Hidden;

  identifier
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>8D_NUMBER}';

  complaint
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>COMPLAINT_ID}';

  supplierIssueProcessesType
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>TYPE}'
  @Common.FieldControl     : #ReadOnly;

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
  @Common.Label            : '{i18n>SUPPLIER}'
  @Common                  : {
      Text            : supplier.businessPartnerNumber,
      TextArrangement : #TextOnly
  };

  material
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>MATERIAL}'
  @Common                  : {
      Text            : material.materialCode,
      TextArrangement : #TextOnly
  };

  unit
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>UNIT}';

  quantity
  @Measures.Unit    : unit
  @UI.HiddenFilter
  @Common.Label            : '{i18n>QUANTITY}';

  status
  @Search.defaultSearchElement
  @Common.Label        : '{i18n>STATUS}'
  @Common              : {
    Text            : status.name,
    TextArrangement : #TextOnly
  };

  plant
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>PLANT}'
  @Common                  : {
    Text            : plant.plant,
    TextArrangement : #TextOnly
  };

  purchasingOrganization
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>PURCHASING_ORGANIZATION}'
  @Common                  : {
    Text            : purchasingOrganization.purchaseOrganizationName,
    TextArrangement : #TextOnly
  };

  company
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>COMPANY}'
  @Common                  : {
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
  @Common.Label            : '{i18n>RESPONSIBLE_PERSON}'
  @Common.FieldControl     : isSupplierFieldControl
  @Common                  : {
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

  requestStartDate
  @Search.defaultSearchElement
  @Common.FieldControl     : isSupplierFieldControl
  @Common.Label            : '{i18n>REQUESTED_START_DATE}';

  requestEndDate
  @Search.defaultSearchElement
  @Common.FieldControl     : isSupplierFieldControl
  @Common.Label            : '{i18n>REQUESTED_END_DATE}';

  actualStartDate
  @Common.FieldControl     : #ReadOnly
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>ACTUAL_START_DATE}';

  actualEndDate
  @Common.FieldControl     : #ReadOnly
  @Search.defaultSearchElement
  @Common.Label            : '{i18n>ACTUAL_END_DATE}';

  defect
  @readonly
  @Search.defaultSearchElement;
}