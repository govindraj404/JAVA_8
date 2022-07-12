using QualityNotificationService from '../../services/index';

annotate QualityNotificationService.Defects with {

    ID
                     @UI.Hidden;

    identifier
                     @Common.Label        : '{i18n>NUMBER}'
                     @Common.FieldControl : #ReadOnly;


    description
                     @UI.MultiLineText
                     @Common.Label        : '{i18n>DESCRIPTION}'
                     @Common.FieldControl : isQualityNotificationFieldControl;


    defectGroup_code @Common              : {
        Label            : '{i18n>GROUP}',
        FieldControl     : isQualityNotificationFieldControlMandatory,
        Text             : defectGroup.description,
        TextArrangement  : #TextOnly,
        ValueListMapping : {
            Label          : '{i18n>GROUP}',
            CollectionPath : 'DefectGroups',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : defectGroup_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                }
            ]
        },
        ValueListWithFixedValues
    }                @Search.defaultSearchElement;

    defectCode_code  @Common              : {
        Label            : '{i18n>CODE}',
        FieldControl     : isQualityNotificationFieldControlMandatory,
        Text             : defectCode.description,
        TextArrangement  : #TextOnly,
        ValueListMapping : {
            Label          : '{i18n>CODE}',
            CollectionPath : 'DefectCodes',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : defectCode_code
                },
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'defectGroup_code',
                    LocalDataProperty : defectGroup_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                }
            ]
        },
        ValueListWithFixedValues
    }                @Search.defaultSearchElement;
}

annotate QualityNotificationService.Defects with @(

Common.SideEffects #defectCode : {
    SourceProperties : [defectCode_code],
    TargetProperties : ['defectGroup_code'],
    TargetEntities   : [
        defectCode,
        defectGroup
    ]
});
