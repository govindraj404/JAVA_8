using ConfigurationService from '../../services/index';

annotate ConfigurationService.ReferenceDocumentCategoryMappings {

    ID                             @UI.Hidden;
    identifier                     @Common.Label : '{i18n>SEQUENCE_NUMBER}';

    salesOrganization_ID           @Common       : {

        Label        : '{i18n>SALES_ORGANIZATION}',
        FieldControl : #Mandatory
    };

    salesOrganization              @Common       : {
        Text             : salesOrganization.salesOrganizationName,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>SALES_ORGANIZATION}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>SALES_ORGANIZATION}',
            CollectionPath : 'SalesAreas',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'salesOrganizationID_ID',
                    LocalDataProperty : salesOrganization_ID
                },
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'distributionChannelID',
                    LocalDataProperty : distributionChannel_ID
                },
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'divisionID',
                    LocalDataProperty : division_ID
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'salesOrganization',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'salesOrganizationName',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'distributionChannel',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'distributionChannelName',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'salesDivision',
                    ![@UI.Importance] : #High
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'salesDivisionName',
                    ![@UI.Importance] : #High
                }
            ]
        }
    };

    distributionChannel_ID         @Common       : {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    distributionChannel            @Common       : {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    division_ID                    @Common       : {
        Label        : '{i18n>DIVISION}',
        FieldControl : #Mandatory
    };

    division                       @Common       : {
        Text            : division.salesDivisionName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DIVISION}',
        FieldControl    : #Mandatory
    };

    referenceDocumentCategory      @Common       : {
        Label           : '{i18n>REFERENCE_CATEGORY}',
        Text            : referenceDocumentCategory.description,
        TextArrangement : #TextOnly,
    };

    referenceDocumentCategory_code @Common.Label : '{i18n>REFERENCE_CATEGORY}';

    sourceSystem                   @Common       : {
        Label            : '{i18n>SOURCE_SYSTEM}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>SOURCE_SYSTEM}',
            CollectionPath : 'Destinations',
            Parameters     : [{
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'destination',
                LocalDataProperty : sourceSystem,
                ![@Common.Label]  : '{i18n>SOURCE_SYSTEM}'
            }]
        }
    };

    sourceReferenceTypeMappings    @UI.Hidden;

    complaintCategory
                                   @Common       : {
        Text             : complaintCategory.name,
        TextArrangement  : #TextOnly,
        FieldControl     : #Mandatory,
        Label            : '{i18n>COMPLAINT_CATEGORY}',
        ValueListMapping : {
            Label          : '{i18n>COMPLAINT_CATEGORY}',
            CollectionPath : 'ComplaintCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : complaintCategory_code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'name'
                }
            ]
        }
    };

    complaintCategory_code         @Commom       : {
        Label        : '{i18n>COMPLAINT_CATEGORY}',
        FieldControl : #Mandatory
    };

    itemCategory
                                   @Common       : {
        Text             : itemCategory.description,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>ITEM_CATEGORY}',
        FieldControl     : #Mandatory,
        ValueListMapping : {
            Label          : '{i18n>ITEM_CATEGORY}',
            CollectionPath : 'ItemCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'ID',
                    LocalDataProperty : itemCategory_ID
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'code'
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                }
            ]
        }
    };

    itemCategory_ID                @Common       : {
        Label        : '{i18n>ITEM_CATEGORY}',
        FieldControl : #Mandatory
    };

    complaintType_ID               @Common       : {Label : '{i18n>COMPLAINT_TYPE}'};

    complaintType
                                   @Common       : {
        Text             : complaintType.description,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>COMPLAINT_TYPE}',
        ValueListMapping : {
            Label          : '{i18n>COMPLAINT_TYPE}',
            CollectionPath : 'ComplaintTypeConfigurations',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'ID',
                    LocalDataProperty : complaintType_ID
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'code'
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'description'
                }
            ]
        }
    };


}
