using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.complaintCategory.ComplaintCategories with {
    code
         @Common : {
        Label : '{i18n>COMPLAINT_CATEGORY}',
        Text  : {
            $value                 : name,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    name @Common : {Label : '{i18n>COMPLAINT_CATEGORY}'};
};


annotate cmh.complaintCategory.ComplaintCategory with @(
    Common.ValueListMapping : {
        Label          : '{i18n>COMPLAINT_CATEGORY}',
        CollectionPath : 'ComplaintCategories',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : complaintType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
        ]
    },
    Common.ValueListWithFixedValues
);
