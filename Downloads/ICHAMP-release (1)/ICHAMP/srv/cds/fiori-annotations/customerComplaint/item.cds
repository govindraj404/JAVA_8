using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.Items with {
    ID                             @UI.Hidden;
    identifier                     @Common.Label : '{i18n>COMPLAINT_NUMBER}';
    itemNumber                     @Common.Label : '{i18n>ITEM_NUMBER}';
    material_ID                    @Common.Label : '{i18n>COMPLAINT_MATERIAL}';
    complaintQuantity              @Common.Label : '{i18n>COMPLAINT_QUANTITY}';
    returnFollowupType_code        @Common.Label : '{i18n>RETURN_FOLLOW_UP_TYPE}';
    returnLocation                 @Common.Label : '{i18n>RETURN_LOCATION}';
    confirmationStatus             @Common.Label : '{i18n>STATUS}';
    netWeight                      @Common.Label : '{i18n>NET_WEIGHT}';
    grossWeight                    @Common.Label : '{i18n>GROSS_WEIGHT}';
    weightUnit                     @Common.Label : '{i18n>WEIGHT_UNIT}';
    volume                         @Common.Label : '{i18n>VOLUME}';
    volumeUnit                     @Common.Label : '{i18n>VOLUME_UNIT}';
    referenceDocumentCategory      @Common.Label : '{i18n>REFERENCE_CATEGORY}';
    referenceDocumentCategory_code @Common.Label : '{i18n>REFERENCE_CATEGORY}';
    referenceDocumentNumber        @Common.Label : '{i18n>REFERENCE_DOCUMENT_NUMBER}';
   // complaintReason_ID             @Common.Label : '{i18n>COMPLAINT_REASON}';
    materialUnknown                @Common.Label : '{i18n>MATERIAL_UNKNOWN}';
    complaintMaterial              @Common.Label : '{i18n>COMPLAINT_MATERIAL}';
    referenceQuantity              @Common.Label : '{i18n>REFERENCE_QUANTITY}';
    receivedQuantity               @Common.Label : '{i18n>RECEIVED_QUANTITY}';
    returnQuantity                 @Common.Label : '{i18n>RETURN_QUANTITY}';
    itemCategory_ID                @Common.Label : '{i18n>ITEM_CATEGORY}';
    itemCategory                   @Common.Label : '{i18n>ITEM_CATEGORY}';
    referenceType_ID               @Common.Label : '{i18n>REFERENCE_TYPE}';
    parentID                       @UI.Hidden;

    // complaintReason                @Common       : {
    //     Label           : '{i18n>COMPLAINT_REASON}',
    //     Text            : complaintReason.description,
    //     TextArrangement : #TextOnly
    // };

    material                       @Common       : {
        Label           : '{i18n>COMPLAINT_MATERIAL}',
        Text            : material.materialCode,
        TextArrangement : #TextOnly
    };

    returnFollowupType             @Common       : {
        Label           : '{i18n>RETURN_FOLLOW_UP_TYPE}',
        Text            : returnFollowupType.description,
        TextArrangement : #TextOnly
    };

    referenceType                  @Common       : {
        Label           : '{i18n>REFERENCE_TYPE}',
        Text            : referenceType.description,
        TextArrangement : #TextOnly
    };

}