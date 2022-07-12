using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.complaintStatus.ComplaintStatuses with {
    code 
    @UI.Hidden : true
    @Common : {
        Label                    : '{i18n>STATUS}',
        Text  : {
            $value                : name,
           ![@UI.TextArrangement]  : #TextOnly
            }
    };
    name @Common : {
        Label        : '{i18n>STATUS}'
    };
};

annotate cmh.complaintStatus.ComplaintStatus  with @(
    Common.ValueListMapping : {
        Label          : '{i18n>STATUS}',
        CollectionPath : 'ComplaintStatuses',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : complaintStatus_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
        ]
    },Common.ValueListWithFixedValues
);