using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.complaintItemCategory.ComplaintItemCategories with {};


@cds.autoexpose
annotate cmh.referenceType.ReferenceTypes with {
    ID
                @Common : {
        Label : '{i18n>REFERENCE_TYPE}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>REFERENCE_TYPE}'};
};


annotate cmh.referenceType.ReferenceType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>REFERENCE_TYPE}',
        CollectionPath : 'ReferenceTypes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : referenceType_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    },
    Common.ValueListWithFixedValues
);


@cds.autoexpose
annotate cmh.complaintReason.ComplaintReasons with {
    ID
                @Common : {
        Label : '{i18n>COMPLAINT_REASON}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>COMPLAINT_REASON}'};
};


annotate cmh.complaintReason.ComplaintReason with @(
    Common.ValueListMapping : {
        Label          : '{i18n>COMPLAINT_REASON}',
        CollectionPath : 'ComplaintReasons',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : complaintReason_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'isActive',
                Constant          : 'true'
            }
        ]
    },
    Common.ValueListWithFixedValues
);


@cds.autoexpose
annotate cmh.targetType.TargetTypes with {
    ID
    @UI.Hidden
    @Common : {
        Label : '{i18n>TARGET_TYPE}',
        Text  : {
            $value                 : code,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    code @Common : {Label : '{i18n>TARGET_TYPE}'};

    description @Common : {Label : '{i18n>DESCRIPTION}'};
};


annotate cmh.targetType.TargetType with @(Common.ValueListMapping : {
    Label          : '{i18n>TARGET_TYPE}',
    CollectionPath : 'TargetTypes',
    Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'ID',
            LocalDataProperty : targetType_ID
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'code'
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'description'
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'targetDocumentCategory_code'
        },
        {
            $Type             : 'Common.ValueListParameterConstant',
            ValueListProperty : 'isActive',
            Constant : 'true'
        }
    ]
});
