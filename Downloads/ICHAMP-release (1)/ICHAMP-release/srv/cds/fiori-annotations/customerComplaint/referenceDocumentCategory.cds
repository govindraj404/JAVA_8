using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.referenceDocumentCategory.ReferenceDocumentCategories with {
    code        @UI.Hidden
                @Common : {
        Label : '{i18n>REFERENCE_CATEGORY}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>REFERENCE_CATEGORY}'};
};


annotate cmh.referenceDocumentCategory.ReferenceDocumentCategory with @(
    Common.ValueListMapping : {
        Label          : '{i18n>REFERENCE_CATEGORY}',
        CollectionPath : 'ReferenceDocumentCategories',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : referenceDocumentCategory_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                Constant          : 'BILLDOC',
                ValueListProperty : 'code'
            }
        ]
    },
    Common.ValueListWithFixedValues
);
