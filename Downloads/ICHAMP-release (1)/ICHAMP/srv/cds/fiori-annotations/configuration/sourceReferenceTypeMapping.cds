using ConfigurationService from '../../services/index';

annotate ConfigurationService.SourceReferenceTypeMappings {

    ID                     @UI.Hidden;
    identifier             @Common.Label : '{i18n>SEQUENCE_NUMBER}';
    complaintType_ID       @Common       : {Label : '{i18n>COMPLAINT_TYPE}'};

    complaintType
                           @Common       : {
        Text             : complaintType.description,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>COMPLAINT_TYPE}',
        FieldControl     : #Mandatory,
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
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'itemCategory_ID',
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

    itemCategory_ID        @Common       : {
        Label        : '{i18n>ITEM_CATEGORY}',
        FieldControl : #Mandatory
    };
         isInActive

                        @UI.Hidden;
         isActive

                         @Common.Label : '{i18n>ACTIVE}';

    salesOrganization_ID   @Common       : {

        Label        : '{i18n>SALES_ORGANIZATION}',
        FieldControl : #Mandatory
    };

    salesOrganization      @Common       : {
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

    distributionChannel_ID @Common       : {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    distributionChannel    @Common       : {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    division_ID            @Common       : {
        Label        : '{i18n>DIVISION}',
        FieldControl : #Mandatory
    };

    division               @Common       : {
        Text            : division.salesDivisionName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DIVISION}',
        FieldControl    : #Mandatory
    };

    sourceSystem           @Common       : {
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

}


annotate ConfigurationService.SourceReferenceTypes {
    referenceType
    @Common       : {
        Label           : '{i18n>REFERENCE_TYPE}',
        Text            : referenceType.description,
        TextArrangement : #TextOnly,
        FieldControl     : SourceReferenceTypeMappings.isFieldControlReferenceType
    };
    
    referenceType_ID
    @Common.Label : '{i18n>REFERENCE_TYPE}';

}
