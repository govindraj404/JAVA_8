using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.complaintQuantityRule.ComplaintQuantityRules with {
    code
         @Common : {
        Label : '{i18n>COMPLAINT_QUANTITY_RULE}',
        Text  : {
            $value                 : name,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    name @Common : {Label : '{i18n>COMPLAINT_QUANTITY_RULE}'};
};


annotate cmh.complaintQuantityRule.ComplaintQuantityRule with @(
    Common.ValueListMapping : {
        Label          : '{i18n>COMPLAINT_QUANTITY_RULE}',
        CollectionPath : 'ComplaintQuantityRules',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : complaintQuantityRule_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
        ]
    },
    Common.ValueListWithFixedValues
);
