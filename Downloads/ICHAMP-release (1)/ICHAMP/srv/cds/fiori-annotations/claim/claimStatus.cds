using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.claimStatus.ClaimStatuses with {
    code
    @UI.Hidden
    @Common : {Text : {
        $value                 : name,
        ![@UI.TextArrangement] : #TextOnly
    }};

    name @Common : {
        Label        : '{i18n>STATUS}'
    };
};

annotate cmh.claimStatus.ClaimStatuses with @(
    Capabilities.SearchRestrictions.Searchable : true,
    UI: {
        PresentationVariant#SortBySequenceNumber :{
            ID : 'SortBySequenceNumber',
            Text: '{i18n>STATUS}',
            SortOrder      : [
                    {
                        Property   : 'sequenceNumber',
                        Descending : false
                    }
                ]
        },
        SelectionVariant#IgnoreNew : {
            ID : 'IgnoreNew',
            Text: '{i18n>STATUS}',
            SelectOptions: [
                {
                    $Type     : 'UI.SelectOptionType',
                    PropertyName : code,
                    Ranges: [
                        {
                            $Type: 'UI.SelectionRangeType',
                            Sign : #I,
                            Option: #NE,
                            Low: 'NEW'
                        }
                    ]
                }
            ]
        },
    }
);

annotate cmh.claimStatus.ClaimStatus  with @(
    Common.ValueListMapping : {
        Label          : '{i18n>STATUS}',
        CollectionPath : 'ClaimStatuses',
        SelectionVariantQualifier: 'IgnoreNew',
        PresentationVariantQualifier : 'SortBySequenceNumber',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : status_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
        ]
    },Common.ValueListWithFixedValues
);