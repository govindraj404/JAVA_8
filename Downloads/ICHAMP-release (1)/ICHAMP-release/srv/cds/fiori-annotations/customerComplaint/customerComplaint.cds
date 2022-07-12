using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.CustomerComplaints with {
    ID                     @UI.Hidden;
    identifier             @Common.Label            :       '{i18n>COMPLAINT_NUMBER}';
    lifeCycleStatus_code   @Common.Label            :       '{i18n>LIFECYCLE_STATUS}';
    lifeCycleStatus        @Common.Label            :       '{i18n>LIFECYCLE_STATUS}';
    individualComplaint    @Common.Label            :       '{i18n>INDIVIDUAL_COMPLAINT}';
    referenceDocument      @Core.Immutable  @Common.Label : '{i18n>REFERENCE_DOCUMENT}';
    referenceDocumentItem  @Core.Immutable  @Common.Label : '{i18n>REFERENCE_DOCUMENT_ITEM}';
    complaintType          @Core.Immutable
                           @Common                  :       {
        Text             : complaintType.description,
        FieldControl     : #Mandatory,
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
                },
                {
                    $Type             : 'Common.ValueListParameterOut',
                    ValueListProperty : 'individualComplaintType',
                    LocalDataProperty : individualComplaint
                }
            ]
        }
    };

    complaintType_ID       @Common.Label            :       '{i18n>COMPLAINT_TYPE}';

    itemCategory           @Core.Immutable
                           @Common                  :       {
        Text             : itemCategory.description,
        FieldControl     : #Mandatory,
        TextArrangement  : #TextOnly,
        Label            : '{i18n>ITEM_CATEGORY}',
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
                },
                {
                    $Type             : 'Common.ValueListParameterIn',
                    ValueListProperty : 'individualComplaint',
                    LocalDataProperty : individualComplaint
                }
            ]
        }
    };

    complaintChannel_ID    @Common.Label            :       '{i18n>CHANNEL_CODE}';
    salesOrganization_ID   @Common                  :       {

        Label        : '{i18n>SALES_ORGANIZATION}',
        FieldControl : #Mandatory
    };

    salesOrganization      @Core.Immutable
                           @Common                  :       {
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

    distributionChannel_ID @Common                  :       {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    distributionChannel    @Core.Immutable
                           @Common                  :       {
        Text            : distributionChannel.distributionChannelName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DISTRIBUTION_CHANNEL}',
        FieldControl    : #Mandatory
    };

    division_ID            @Common                  :       {
        Label        : '{i18n>DIVISION}',
        FieldControl : #Mandatory
    };

    division               @Core.Immutable
                           @Common                  :       {
        Text            : division.salesDivisionName,
        TextArrangement : #TextOnly,
        Label           : '{i18n>DIVISION}',
        FieldControl    : #Mandatory
    };
    externalReference      @Common.Label            :       '{i18n>EXTERNAL_REFERENCE}';
    complaintCategory      @Common.Label            :       '{i18n>COMPLAINT_CATEGORY}';
    complaintCategory_code @Common.Label            :       '{i18n>COMPLAINT_CATEGORY}';
    createdBy              @UI.HiddenFilter         :       false;

    complaintChannel       @Common                  :       {
        Label           : '{i18n>CHANNEL_CODE}',
        Text            : complaintChannel.description,
        TextArrangement : #TextOnly
    };

    soldToParty
                           @Common.ValueListMapping :       {
        Label          : '{i18n>SOLD_TO_PARTY}',
        CollectionPath : 'BusinessPartners',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : soldToParty_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerNumber',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName1',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'businessPartnerName2',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/houseNumber',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/address',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/city',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/postalCode',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/country',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/mobile',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/email',
                ![@UI.Importance] : #Medium
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'isMarkedForDeletion',
                Constant          : 'false'
            }
        ]
    }
                           @Search.defaultSearchElement
                           @Common.Label            :       '{i18n>SOLD_TO_PARTY}'
                           @Common                  :       {
        Text            : soldToParty.businessPartnerNumber,
        TextArrangement : #TextOnly
    };

    complaintReason                @Common       : {
        Label           : '{i18n>COMPLAINT_REASON}',
        Text            : complaintReason.description,
        TextArrangement : #TextOnly
    };
    
    complaintReason_ID             @Common.Label : '{i18n>COMPLAINT_REASON}';

    adjustmentValue  @Common.Label : '{i18n>COMPLAINT_REASON}';

    rejectionReason_ID @Common.Label : '{i18n>REJECTION_REASON}';
    rejectionReason @Common.Label : '{i18n>REJECTION_REASON}';
    confirmationStatus_code @Common.Label : '{i18n>CONFIRMATION_STATUS}';
    confirmationStatus @Common.Label : '{i18n>CONFIRMATION_STATUS}';

}
