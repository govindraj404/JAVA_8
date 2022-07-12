using SupplierIssueProcessService from '../../services/index';

annotate SupplierIssueProcessService.Defects with {

    ID
                     @UI.Hidden;

    identifier
                     @Common.Label        : '{i18n>NUMBER}'
                     @Common.FieldControl : #ReadOnly;

    defectGroup_code @Common              : {
        Label           : '{i18n>GROUP}',
        FieldControl    : #ReadOnly,
        Text            : defectGroup.description,
        TextArrangement : #TextOnly
    }
                     @Search.defaultSearchElement;

    defectCode_code  @Common              : {
        Label           : '{i18n>CODE}',
        FieldControl    : #ReadOnly,
        Text            : defectCode.description,
        TextArrangement : #TextOnly
    }
                     @Search.defaultSearchElement;

    description
                     @UI.MultiLineText
                     @Common.Label        : '{i18n>DESCRIPTION}'
                     @Common.FieldControl : #ReadOnly;

}
