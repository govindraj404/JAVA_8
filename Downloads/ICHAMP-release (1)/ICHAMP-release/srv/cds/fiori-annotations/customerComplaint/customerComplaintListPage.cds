using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.CustomerComplaints with @(
    odata.draft.enabled : true,
    Capabilities        : {
        Deletable : false
        
        
        
        },
    Common.SemanticKey  : [
        identifier,
        complaintCategory_code
    ],
    UI                  : {
        PresentationVariant : {
            Visualizations : ['@UI.LineItem'],
            SortOrder      : [{
                Property   : 'createdAt',
                Descending : true
            }]
        },
        SelectionFields     : [
            identifier,
            complaintType_ID,
            itemCategory_ID,
            //combineItems.itemCategory_ID,
            lifeCycleStatus_code,
            soldToParty_ID,
            combineItems.referenceDocumentCategory_code,
            combineItems.referenceDocumentNumber,
            combineItems.material_ID,
            createdBy
        ],
        LineItem            : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : lifeCycleStatus_code,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : complaintType_ID,
                ![@UI.Importance] : #Low
            },
            {
                $Type             : 'UI.DataField',
                Value             : itemCategory_ID,
                ![@UI.Importance] : #Low
            },
            {
                $Type             : 'UI.DataField',
                Value             : complaintReason_ID,
                Label             : '{i18n>COMPLAINT_REASON}',
                ![@UI.Importance] : #Low
            },
            {
                $Type             : 'UI.DataField',
                Value             : soldToParty_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'UI.DataField',
                Value             : salesOrganization_ID,
                ![@UI.Importance] : #High
            }
        ]
    }
);
