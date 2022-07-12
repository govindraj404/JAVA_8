using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.Notes with {
    note
             @Common.Label : '{i18n>NOTE}';

    noteType_code
             @Common.Label : '{i18n>NOTE_TYPE}';

    ID
             @UI.Hidden;

    parentID
             @UI.Hidden;

    noteType @Common       : {
        Label           : '{i18n>NOTE_TYPE}',
        Text            : noteType.description,
        TextArrangement : #TextOnly
    };
}
