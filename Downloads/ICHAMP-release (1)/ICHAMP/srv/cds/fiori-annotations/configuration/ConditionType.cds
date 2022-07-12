using ConfigurationService from '../../services/index';

annotate ConfigurationService.ConditionTypes with {
    
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
    @Common.Label : '{i18n>ITEM_TYPE}'
    @Common.FieldControl : #Mandatory;

    conditionType
    @Common.Label : '{i18n>CONDITION_TYPE}'
    @Common.FieldControl : #Mandatory;

    description
    @Common.Label : '{i18n>DESCRIPTION}';
    
    itemType  @Common : 
        {
            Text            : itemType.description,
            TextArrangement : #TextOnly
        };
}