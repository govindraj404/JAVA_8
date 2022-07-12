using ComplaintService from '../../services/index';

annotate ComplaintService.BusinessObjects with @(
    Common.SemanticKey : [ID],
    Capabilities       : {FilterRestrictions : {FilterExpressionRestrictions : [{
        Property           : businessObjectID_ID,
        AllowedExpressions : 'MultiValue'
    }]}},
    UI                 : {
        HeaderInfo                              : {
            TypeNamePlural : '{i18n>ITEMS}',
            TypeName       : '{i18n>ITEMS}'
        },
        SelectionPresentationVariant #Relevant  : {
            ID                  : 'Relevant',
            Text                : '{i18n>RELEVANT}',
            PresentationVariant : {
                Visualizations : ['@UI.LineItem'],
                RequestAtLeast : [
                    stream.sequenceNumber,
                    businessObjectType.sequenceNumber
                ],
                SortOrder      : [
                    {
                        Property   : 'stream/sequenceNumber',
                        Descending : true
                    },
                    {
                        Property   : 'businessObjectType/sequenceNumber',
                        Descending : false
                    }
                ]
            },
            // SelectionVariant    : {
            SelectOptions       : [
                {
                    $Type        : 'UI.SelectOptionType',
                    PropertyName : IsActiveEntity,
                    Ranges       : [{
                        $Type  : 'UI.SelectionRangeType',
                        Sign   : #I,
                        Option : #EQ,
                        Low    : true
                    }]
                },
                {
                    $Type        : 'UI.SelectOptionType',
                    PropertyName : isRelevant,
                    Ranges       : [{
                        $Type  : 'UI.SelectionRangeType',
                        Sign   : #I,
                        Option : #EQ,
                        Low    : true
                    }]
                }
            ]
        // }
        },
        SelectionPresentationVariant #All       : {
            ID                  : 'All',
            Text                : '{i18n>ALL}',
            PresentationVariant : {
                Visualizations : ['@UI.LineItem'],
                SortOrder      : [
                    {
                        Property   : 'stream/sequenceNumber',
                        Descending : true
                    },
                    {
                        Property   : 'businessObjectType/sequenceNumber',
                        Descending : false
                    }
                ]
            },
            //SelectionVariant    : {
            SelectOptions       : [{
                $Type        : 'UI.SelectOptionType',
                PropertyName : IsActiveEntity,
                Ranges       : [{
                    $Type  : 'UI.SelectionRangeType',
                    Sign   : #I,
                    Option : #EQ,
                    Low    : true
                }]
            }]
        //}
        },
        SelectionPresentationVariant #Alls      : {
            ID                  : 'All',
            Text                : '{i18n>ALL}',
            PresentationVariant : {
                Visualizations : ['@UI.LineItem'],
                SortOrder      : [
                    {
                        Property   : 'stream/sequenceNumber',
                        Descending : true
                    },
                    {
                        Property   : 'businessObjectType/sequenceNumber',
                        Descending : false
                    }
                ]
            },
            // SelectionVariant    : {
            SelectOptions       : [{
                $Type        : 'UI.SelectOptionType',
                PropertyName : IsActiveEntity,
                Ranges       : [{
                    $Type  : 'UI.SelectionRangeType',
                    Sign   : #I,
                    Option : #EQ,
                    Low    : false
                }]
            }]
        // }
        },
        SelectionPresentationVariant #Relevants : {
            ID                  : 'Relevant',
            Text                : '{i18n>RELEVANT}',
            PresentationVariant : {
                Visualizations : ['@UI.LineItem'],
                SortOrder      : [
                    {
                        Property   : 'stream/sequenceNumber',
                        Descending : true
                    },
                    {
                        Property   : 'businessObjectType/sequenceNumber',
                        Descending : false
                    }
                ]
            },
            // SelectionVariant    : {
            SelectOptions       : [
                {
                    $Type        : 'UI.SelectOptionType',
                    PropertyName : IsActiveEntity,
                    Ranges       : [{
                        $Type  : 'UI.SelectionRangeType',
                        Sign   : #I,
                        Option : #EQ,
                        Low    : false
                    }]
                },
                {
                    $Type        : 'UI.SelectOptionType',
                    PropertyName : isRelevant,
                    Ranges       : [{
                        $Type  : 'UI.SelectionRangeType',
                        Sign   : #I,
                        Option : #EQ,
                        Low    : true
                    }]
                }
            ]
        // }
        },
        LineItem                                : [
            {
                $Type : 'UI.DataField',
                Label : '{i18n>STREAMS}',
                Value : ID
            },
            {
                $Type : 'UI.DataField',
                Label : '{i18n>STREAM_STATUS}',
                Value : stream.status.name
            },
            {
                $Type             : 'UI.DataField',
                Label             : '{i18n>RESPONSIBLE_PERSON}',
                Value             : businessObjectID.personResponsible_ID,
                ![@UI.Importance] : #Medium
            }
        ]
    }
);