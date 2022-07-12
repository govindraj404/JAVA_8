using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintReasons {

    ID
    @UI.Hidden;

    identifier
    @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    code
    @Common : {
            FieldControl : #Mandatory,
            Label : '{i18n>REASON_CODE}'
    };

    description
    @Common.Label : '{i18n>DESCRIPTION}';

    isInActive
    @UI.Hidden;

    isActive
    @Common.Label : '{i18n>ACTIVE}';

   }
