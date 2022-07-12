using com.sap.ic.cmh as cmh from '../../../../db/cds/index';


@cds.autoexpose
annotate cmh.noteType.NoteTypes with {
    code
                @Common : {
        Label : '{i18n>NOTE_TYPE}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>DESCRIPTION}'};
};


annotate cmh.noteType.NoteType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>NOTE_TYPE}',
        CollectionPath : 'NoteTypes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : noteType_code
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
annotate cmh.referenceType.ReturnFollowUpTypes with {
    code
                @Common : {
        Label : '{i18n>RETURN_FOLLOW_UP_TYPE}',
        Text  : {
            $value                 : description,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    description @Common : {Label : '{i18n>RETURN_FOLLOW_UP_TYPE}'};
};


annotate cmh.referenceType.ReturnFollowUpType with @(
    Common.ValueListMapping : {
        Label          : '{i18n>RETURN_FOLLOW_UP_TYPE}',
        CollectionPath : 'ReturnFollowUpTypes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : returnFollowupType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
        ]
    },
    Common.ValueListWithFixedValues
);
