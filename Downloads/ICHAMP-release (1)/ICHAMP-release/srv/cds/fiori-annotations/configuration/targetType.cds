using ConfigurationService from '../../services/index';

annotate ConfigurationService.TargetTypes {

    ID
    @UI.Hidden
    @Common       : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : code,
            ![@UI.TextArrangement] : #TextOnly
        }
    };


    identifier
                                @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    code
     @Common : {
        Label           : '{i18n>TARGET_TYPE}',
        FieldControl    : #Mandatory
    };

    description
                                @Common.Label : '{i18n>DESCRIPTION}';

    targetDocumentCategory      @Common       : {
        Label           : '{i18n>TARGET_DOCUMENT_CATEGORY}',
        Text            : targetDocumentCategory.description,
        TextArrangement : #TextOnly,
        FieldControl    : #Mandatory
    };

    targetDocumentCategory_code @Common.Label : '{i18n>TARGET_DOCUMENT_CATEGORY}';

    isInActive
    @UI.Hidden;

    isActive
    @Common.Label : '{i18n>ACTIVE}';

}
