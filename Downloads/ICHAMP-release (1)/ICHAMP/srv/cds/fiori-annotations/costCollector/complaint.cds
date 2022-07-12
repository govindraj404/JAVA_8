using CostCollectorService from '../../services/index';

annotate CostCollectorService.Complaints with {
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
    @UI.MultiLineText
    @Common.Label         : '{i18n>DESCRIPTION}';

    referenceNumber
    @Common.Label         : '{i18n>REFERENCE_NUMBER}';

    company
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>COMPANY_CODE}';

    note
    @UI.MultiLineText
    @Common.Label         : '{i18n>NOTE}';

    personResponsible_ID
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>RESPONSIBLE_PERSON}';

    contactPerson
    @Common.Label         : '{i18n>CONTACT_PERSON}';

    creationType
    @Common.Label         : '{i18n>CREATION_TYPE}'
    @Common.FieldControl  : #ReadOnly;

    createdAt
    @Search.defaultSearchElement
    @UI.HiddenFilter      : false;

    createdBy
    @Search.defaultSearchElement
    @UI.HiddenFilter      : false;

    modifiedAt
    @Search.defaultSearchElement
    @UI.HiddenFilter      : false;

    modifiedBy
    @Search.defaultSearchElement
    @UI.HiddenFilter      : false;

    businessObjects
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>BUSINESS_OBJECT_NUMBER}';

    unit
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>UNIT}';

    quantity
    @Measures.Unit : unit_code
    @Common.Label         : '{i18n>QUANTITY}';

    purchasingOrganization
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>PURCHASING_ORGANIZATION}';

    plant
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>PLANT}';

    supplier
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>SUPPLIER}';

    material
    @Search.defaultSearchElement
    @Common.Label         : '{i18n>MATERIAL}';

    laborUnit_code
    @Common.Label         : '{i18n>LABOR_UNIT}';

    totalLaborHour
    @Common.Label         : '{i18n>TOTAL_LABOR}'
    @Measures.Unit        : laborUnit_code;

    currency_code
    @Common.Label         : '{i18n>CURRENCY}';

    totalSubLetCost
    @Common.Label         : '{i18n>TOTAL_SUBLET_COST}'
    @Measures.ISOCurrency : currency_code;

}