using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.targetDocumentCategory.TargetDocumentCategories with {
    code
                @Common : {
        Label : '{i18n>TARGET_DOCUMENT_CATEGORY}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>TARGET_DOCUMENT_CATEGORY}'};
};


annotate cmh.targetDocumentCategory.TargetDocumentCategory with @(
    Common.ValueListMapping : {
        Label          : '{i18n>TARGET_DOCUMENT_CATEGORY}',
        CollectionPath : 'TargetDocumentCategories',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : targetDocumentCategory_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    },
    Common.ValueListWithFixedValues
);