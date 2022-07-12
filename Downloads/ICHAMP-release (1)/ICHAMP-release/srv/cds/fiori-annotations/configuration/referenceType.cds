using ConfigurationService from '../../services/index';

annotate ConfigurationService.ReferenceTypes {

    ID
                                   @UI.Hidden;

    identifier
                                   @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    code
    @Common : {
        Label           : '{i18n>REFERENCE_TYPE}',
        FieldControl    : #Mandatory
    };

    description
                                   @Common.Label : '{i18n>DESCRIPTION}';

    referenceDocumentCategory      @Common       : {
        Label           : '{i18n>REFERENCE_CATEGORY}',
        Text            : referenceDocumentCategory.description,
        FieldControl    : #Mandatory,
        TextArrangement : #TextOnly
    };

    referenceDocumentCategory_code @Common.Label : '{i18n>REFERENCE_CATEGORY}';
     isInActive

                    @UI.Hidden;
     isActive
                    @Common.Label : '{i18n>ACTIVE}';

}