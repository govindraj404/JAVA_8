using ConfigurationService from '../../services/index';

annotate ConfigurationService.TargetReferenceTypes {

    ID                @UI.Hidden;

    destinationSystem @Common       : {
        Label            : '{i18n>DESTINATION_SYSTEM}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>DESTINATION_SYSTEM}',
            CollectionPath : 'Destinations',
            Parameters     : [{
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'destination',
                LocalDataProperty : destinationSystem,
                ![@Common.Label]  : '{i18n>DESTINATION_SYSTEM}'
            }]
        }
    };

    targetType        @Common       : {
        Label           : '{i18n>TARGET_TYPE}',
        Text            : targetType.code,
        TextArrangement : #TextOnly
    };

    targetType_ID     @Common.Label : '{i18n>TARGET_TYPE}';
}
