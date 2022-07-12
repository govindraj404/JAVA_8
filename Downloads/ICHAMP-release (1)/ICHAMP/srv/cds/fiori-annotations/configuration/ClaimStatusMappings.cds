using ConfigurationService from '../../services/index';

annotate ConfigurationService.ClaimStatusMappings with {
    ID
           @UI.Hidden;

    identifier
           @Common.Label        : '{i18n>SEQUENCE_NUMBER}';

    code
           @Common.FieldControl : #Mandatory
           @Common.Label        : '{i18n>BACKEND_STATUS_CODE}';

    name
           @Common.FieldControl : #Mandatory
           @Common.Label        : '{i18n>BACKEND_STATUS_NAME}';

    status
           @Common.FieldControl : #Mandatory
           @Common.Label        : '{i18n>COCKPIT_STATUS}';

    status_code
           @Common.FieldControl : #Mandatory
           @Common.Label        : '{i18n>COCKPIT_STATUS}';

    status @Common              : {
        Text            : status.name,
        TextArrangement : #TextOnly
    };
}
