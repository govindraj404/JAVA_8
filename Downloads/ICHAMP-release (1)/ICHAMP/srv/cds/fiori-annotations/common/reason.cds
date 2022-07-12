using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.reason.Reasons with {
    code 
    @UI.Hidden
    @Common : {
        Label                    : '{i18n>REASON}',
        Text  : {
            $value                : description,
           ![@UI.TextArrangement]  : #TextOnly
            }
    };
    description @Common : {
        Label        : '{i18n>DESCRIPTION}'
    };
};

annotate cmh.reason.Reason with @(
    Common.ValueListMapping : {
        Label          : '{i18n>REASON}',
        CollectionPath : 'Reasons',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : reason_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    },Common.ValueListWithFixedValues
);