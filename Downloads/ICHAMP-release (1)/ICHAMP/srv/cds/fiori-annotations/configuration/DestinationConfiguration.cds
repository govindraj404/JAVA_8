using ConfigurationService from '../../services/index';

annotate ConfigurationService.DestinationConfigurations with {
  ID
                     @UI.Hidden;

  identifier
                     @Common.Label            : '{i18n>SEQUENCE_NUMBER}';

  businessObjectType
                     @Common.FieldControl     : #Mandatory
                     @Common.Label            : '{i18n>BUSINESS_OBJECT_TYPE}';

  companyCode_ID
                     @Common.FieldControl     : #Mandatory
                     @Common.Label            : '{i18n>COMPANY_CODE}';

  destination
                     @Common.Label            : '{i18n>DESTINATION}'
                     @Common.FieldControl     : #Mandatory
                     @Common.ValueListMapping : {
    Label          : '{i18n>DESTINATION}',
    CollectionPath : 'Destinations',
    Parameters     : [{
      $Type             : 'Common.ValueListParameterInOut',
      ValueListProperty : 'destination',
      LocalDataProperty : destination
    }]
  };

  companyCode
                     @Common.Label            : '{i18n>COMPANY_CODE}'
                     @Common.FieldControl     : #Mandatory;

  companyCodeName
                     @Common.FieldControl     : #Mandatory
                     @Common.Label            : '{i18n>NAME}';

  description
                     @Common.Label            : '{i18n>DESCRIPTION}';

  navigationDestination
                     @Common.Label            : '{i18n>NAVIGATION_DESTINATION}'
                     @Common.ValueListMapping : {
    Label          : '{i18n>DESTINATION}',
    CollectionPath : 'Destinations',
    Parameters     : [{
      $Type             : 'Common.ValueListParameterInOut',
      ValueListProperty : 'destination',
      LocalDataProperty : navigationDestination
    }]
  };

  companyCode        @Common                  : {
    Text            : companyCode.companyCode,
    TextArrangement : #TextOnly
  };

  businessObjectType @Common                  : {
    Text            : businessObjectType.name,
    TextArrangement : #TextOnly
  };

}
