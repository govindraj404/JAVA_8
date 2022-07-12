using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.defectGroup.DefectGroups with {
    code

                @Common : {
        Label : '{i18n>GROUP}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>DESCRIPTION}'};
};

annotate cmh.defectGroup.DefectGroup with @(
    Common.ValueListMapping : {
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
    Common.ValueListWithFixedValues
);
