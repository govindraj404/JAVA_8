using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.MultipleItems with @(UI : {LineItem : [
    {
        $Type             : 'UI.DataField',
        Value             : itemNumber,
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'UI.DataField',
        Value             : material_ID,
        ![@UI.Importance] : #High,
        Label             : '{i18n>COMPLAINT_MATERIAL}'
    },
    {
        $Type             : 'UI.DataField',
        Value             : complaintQuantity,
        ![@UI.Importance] : #Low
    },
    {
        $Type             : 'UI.DataField',
        Value             : returnFollowupType_code,
        ![@UI.Importance] : #Low
    },
    {
        $Type             : 'UI.DataField',
        Value             : returnLocation,
        ![@UI.Importance] : #Low
    },
    {
        $Type             : 'UI.DataField',
        Value             : confirmationStatus_code,
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'UI.DataField',
        Value             : netWeight,
        ![@UI.Importance] : #High
    }
]});
