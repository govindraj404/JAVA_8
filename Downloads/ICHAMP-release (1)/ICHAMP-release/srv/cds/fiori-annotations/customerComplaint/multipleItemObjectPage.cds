using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.MultipleItems with @(UI : {
    HeaderInfo                           : {
        TypeName       : '{i18n>ITEM}',
        TypeNamePlural : '{i18n>ITEMS}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>ITEM_NUMBER}',
            Value : itemNumber
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>COMPLAINT_MATERIAL}',
            Value : material_ID
        },
    },
    Facets                               : [
        {
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>ITEM_OVERVIEW}',
            ID     : 'ItemComplaintOverviewFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionItemComplaintMainDetails',
                Label  : '{i18n>MAIN_DETAILS}',
                Target : '@UI.FieldGroup#ItemComplaintMainDetails'
            }]
        },
        {
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>ADDITIONAL_DETAILS}',
            ID     : 'AdditionalDetailsFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionAdditionalDetails',
                Label  : '{i18n>AdditionalDetails}',
                Target : '@UI.FieldGroup#AdditionalDetails'
            }]
        },
        {
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>ADMINISTRATIVE_DATA}',
            ID     : 'AdministrativeDataFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionAdminData',
                Label  : '{i18n>INTERNAL_DETAILS}',
                Target : '@UI.FieldGroup#InternalDetails'
            }]
        }
    ],
    FieldGroup #ItemComplaintMainDetails : {Data : [
        {
            $Type : 'UI.DataField',
            Value : parentID.soldToParty_ID
        },
        {
            $Type : 'UI.DataField',
            Value : material_ID,
            Label : '{i18n>COMPLAINT_MATERIAL}'
        },
        {
            $Type : 'UI.DataField',
            Value : materialUnknown
        },
        {
            $Type : 'UI.DataField',
            Value : complaintMaterial
        },
        {
            $Type : 'UI.DataField',
            Value : referenceQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : receivedQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : returnQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : complaintQuantity
        },
        // {
        //     $Type : 'UI.DataField',
        //     Value : returnFollowupType_code
        // },
        {
            $Type : 'UI.DataField',
            Value : returnLocation
        },
        {
            $Type : 'UI.DataField',
            Value : parentID.externalReference
        },
        {
            $Type : 'UI.DataField',
            Value : confirmationStatus_code
        },
        {
            $Type : 'UI.DataField',
            Value : parentID.complaintChannel_ID
        }
    ]},
    FieldGroup #AdditionalDetails        : {Data : [
        {
            $Type : 'UI.DataField',
            Value : grossWeight
        },
        {
            $Type : 'UI.DataField',
            Value : netWeight
        },
        {
            $Type : 'UI.DataField',
            Value : weightUnit
        },
        {
            $Type : 'UI.DataField',
            Value : volume
        },
        {
            $Type : 'UI.DataField',
            Value : volumeUnit
        }
    ]},
    FieldGroup #InternalDetails          : {Data : [
        {
            $Type : 'UI.DataField',
            Value : createdAt
        },
        {
            $Type : 'UI.DataField',
            Value : createdBy
        },
        {
            $Type : 'UI.DataField',
            Value : modifiedAt
        },
        {
            $Type : 'UI.DataField',
            Value : modifiedBy
        }
    ]}
});
