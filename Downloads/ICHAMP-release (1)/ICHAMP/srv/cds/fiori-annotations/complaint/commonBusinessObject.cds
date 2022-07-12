using ComplaintService from '../../services/index';

annotate ComplaintService.CombineBusinessObjectStatuses with @(
    UI : {
         PresentationVariant#SortBy :{
            ID : 'SortBy',
            SortOrder      : [
                    {
                        Property   : 'streamTypeCode',
                        Descending : true
                    },
                    {
                        Property   : 'BOSequenceNumber',
                        Descending : false
                    },
                    {
                        Property   : 'sequenceNumber',
                        Descending : false
                    }
                ]
        },
    SelectionVariant #IgnoreNew : {
    ID            : 'IgnoreNew',
    Text          : '{i18n>STATUS}',
    SelectOptions : [{
        $Type        : 'UI.SelectOptionType',
        PropertyName : code,
        Ranges       : [{
            $Type  : 'UI.SelectionRangeType',
            Sign   : #I,
            Option : #NE,
            Low    : 'NEW'
        }]
    }]
}});

annotate ComplaintService.CommonBusinessObjects with {
    ID
    @Common.Label : '{i18n>BUSINESS_OBJECT_NUMBER}';

    identifier
    @Search.defaultSearchElement
    @Common.Label : '{i18n>BUSINESS_OBJECT_NUMBER}';

    personResponsible @Common                  : {
        Text            : personResponsible.businessPartnerName1,
        TextArrangement : #TextOnly
    };

    status
    @Common.Label  : '{i18n>BUSINESS_OBJECT_LATEST_STATUS}'
    @Common.ValueListMapping : {
        Label          : '{i18n>BUSINESS_OBJECT_LATEST_STATUS}',
        CollectionPath : 'CombineBusinessObjectStatuses',
        SelectionVariantQualifier: 'IgnoreNew',
        PresentationVariantQualifier : 'SortBy',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : status_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name',
                ![@UI.Importance] : #High
            }
        ]
    };
};