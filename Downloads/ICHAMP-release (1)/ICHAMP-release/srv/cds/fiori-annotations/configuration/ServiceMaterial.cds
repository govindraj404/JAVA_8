using ConfigurationService from '../../services/index';

annotate ConfigurationService.ServiceMaterials with {
    ID
    @UI.Hidden;

    identifier
    @Common.Label : '{i18n>SEQUENCE_NUMBER}';
    
    destination
    @Common.Label : '{i18n>DESTINATION}'
    @Common.FieldControl : #Mandatory
    @Common.ValueListMapping : {
        Label          : '{i18n>DESTINATION}',
        CollectionPath : 'Destinations',
        Parameters     : [
            {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'destination',
            LocalDataProperty : destination
        }
      ]
    }; 

    itemType
    @Common.Label  : '{i18n>ITEM_TYPE}'
     @Common.FieldControl : #Mandatory;

    subItemType  
    @Common.Label  : '{i18n>SUB_ITEM_TYPE}'
    @Common.FieldControl : #Mandatory;

    serviceMaterial
    @Common.Label : '{i18n>MATERIAL}'
    @Common.FieldControl : #Mandatory;

    description
    @Common.Label : '{i18n>DESCRIPTION}';

    itemType  @Common : 
        {
            Text            : itemType.description,
            TextArrangement : #TextOnly
        };

        subItemType  @Common : 
        {
            Text            : subItemType.description,
            TextArrangement : #TextOnly
        };
}