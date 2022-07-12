using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.defectCode.DefectCodes with {
    code
                @UI.Hidden
                @Common : {
        Label : '{i18n>CODE}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>DESCRIPTION}'};

    defectGroup_code
                @UI.Hidden
                @Common : {Text : {
        $value                 : defectGroup.description,
        ![@UI.TextArrangement] : #TextOnly
    }};
}


annotate cmh.defectCode.DefectCode with @(
    Common.ValueListMapping : {
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
    Common.ValueListWithFixedValues
);
