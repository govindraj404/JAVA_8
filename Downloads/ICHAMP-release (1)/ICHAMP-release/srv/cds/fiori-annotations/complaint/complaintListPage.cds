using ComplaintService from '../../services/index';

annotate ComplaintService.Complaints with @(
    odata.draft.enabled : true,
    Capabilities : {
        Deletable : false,
        SortRestrictions.NonSortableProperties : [plant.plantName,companyCode.companyCodeName,supplier.businessPartnerName1,contactPerson.businessPartnerName1, complaintStatus.name], 
        FilterRestrictions : {
            NonFilterableProperties      : [
                note,description,creationType,complaintType_code,isComplaintNew,isHideCostCollection,unit_code,contactPerson_ID,
                isHideAdaptStreams,isFieldControlMandatory,isEventSpecificRequest,isHideDiscardComplaint,
                isHideReopenComplaint,isHideCloseComplaint,laborUnit_code
            ],
            FilterExpressionRestrictions : [
                {
                    Property           : createdAt,
                    AllowedExpressions : 'SingleRange'
                },
                {
                    Property           : modifiedAt,
                    AllowedExpressions : 'SingleRange'
                },
                {
                    Property           : companyCode_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : supplier_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : material_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : plant_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : personResponsible_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : businessObjects.businessObjectID_ID,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : currency_code,
                    AllowedExpressions : 'MultiValue'
                },
                {
                    Property           : purchasingOrganization_ID,
                    AllowedExpressions : 'MultiValue'
                }
            ]
        }
    },
    Common.SemanticKey : [identifier],
    UI   : {
        PresentationVariant : {
            RequestAtLeast:[isHideAdaptStreams,isHideReopenComplaint,isHideCloseComplaint,isHideDiscardComplaint],
            Visualizations : ['@UI.LineItem'],
            SortOrder : [
                {
                    Property   : 'createdAt',
                    Descending : true
                }
            ]
            },
        SelectionFields     : [
            identifier,companyCode_ID,complaintStatus_code,referenceNumber,supplier_ID,material_ID,
            plant_ID,personResponsible_ID,createdAt,businessObjects.businessObjectID_ID,
            businessObjects.businessObjectID.status_code
        ],
        LineItem : [
            {
                $Type : 'UI.DataField',
                Value : identifier,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : complaintStatus_code,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : referenceNumber,
                ![@UI.Importance] : #Low
            },
            {
                $Type : 'UI.DataField',
                Value : supplier_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : material_ID,
                ![@UI.Importance] : #High
            },
            {
                $Type : 'UI.DataField',
                Value : plant_ID,
                ![@UI.Importance] : #Medium
            },
            {
                $Type : 'UI.DataField',
                Value : personResponsible_ID,
                ![@UI.Importance] : #Medium
            },
            // {
            //     $Type  : 'UI.DataFieldForAction',
            //     Label  : '{i18n>CLOSE_COMPLAINT}',
            //     Action : 'ComplaintService.Close'
            // },
            // {
            //     $Type  : 'UI.DataFieldForAction',
            //     Label  : '{i18n>REOPEN_COMPLAINT}',
            //     Action : 'ComplaintService.Reopen'
            // }
        ]
    }
);