using ComplaintService from '../../services/index';

annotate ComplaintService.BusinessObjects with {
    ID
    @Common : {
        Label                    : '{i18n>STREAMS}',
        Text  : {
            $value                : stream.streamType.name,
           ![@UI.TextArrangement]  : #TextOnly
        }
    };

    stream
    @UI.Hidden;

    isValidActionPrecondition
    @UI.Hidden;

    complaint
    @UI.Hidden;

    isRelevant
    @Common.Label  : '{i18n>RELEVANT}';

    businessObjectType
    @Common.Label  : '{i18n>BUSINESS_OBJECT_TYPE}';
    
    businessObjectID
    @Common.Label  : '{i18n>BUSINESS_OBJECT_NUMBER}'
    @Common.ValueListMapping : {
    Label          : '{i18n>BUSINESS_OBJECT_NUMBER}',
    CollectionPath : 'CommonBusinessObjects',
    Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'ID',
            LocalDataProperty : businessObjectID_ID
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'identifier',
            ![@UI.Importance] : #High
        }
        ]
    };

    businessObjectID @Common : {
        Text            : businessObjectID.identifier,
        TextArrangement : #TextOnly
    };

    isVisible
    @UI.Hidden;

}