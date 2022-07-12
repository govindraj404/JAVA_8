using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.CustomerComplaints with @(UI : {
    HeaderInfo                             : {
        TypeName       : '{i18n>COMPLAINT}',
        TypeNamePlural : '{i18n>COMPLAINTS}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>CODE}',
            Value : identifier
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>DESCRIPTION}',
            Value : complaintType_ID
        },
    },
    HeaderFacets                           : [
        {
            $Type  : 'UI.ReferenceFacet',
            Label  : '{i18n>GENERAL_INFORMATION}',
            Target : '@UI.FieldGroup#GeneralInformation'
        },
        {
            $Type  : 'UI.ReferenceFacet',
            Label  : '{i18n>CONTACT_INFORMATION}',
            Target : '@UI.FieldGroup#ContactInformation',
        },
        {
            $Type  : 'UI.ReferenceFacet',
            Target : '@UI.DataPoint#Status'
        }
    ],
    Facets                                 : [
        {
            $Type         : 'UI.CollectionFacet',
            Label         : '{i18n>COMPLAINT_OVERVIEW}',
            ID            : 'SingleComplaintOverviewFacet',
            ![@UI.Hidden] : {$edmJson : {$Not : {$Path : 'individualComplaint'}}},
            Facets        : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionSingleComplaintMainDetails',
                Label  : '{i18n>MAIN_DETAILS}',
                Target : '@UI.FieldGroup#SingleComplaintMainDetails'
            }]
        },
        {
            $Type         : 'UI.CollectionFacet',
            Label         : '{i18n>COMPLAINT_OVERVIEW}',
            ID            : 'MultiComplaintOverviewFacet',
            ![@UI.Hidden] : individualComplaint,
            Facets        : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionMultiComplaintMainDetails',
                Label  : '{i18n>MAIN_DETAILS}',
                Target : '@UI.FieldGroup#MultiComplaintMainDetails'
            }]
        },
        {
            $Type         : 'UI.CollectionFacet',
            Label         : '{i18n>COMPLAINT_ITEMS}',
            ID            : 'ItemsFacet',
            ![@UI.Hidden] : individualComplaint,
            Facets        : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'items',
                Label  : '{i18n>ITEMS}',
                Target : 'items/@UI.LineItem'
            }]
        },
        {
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>NOTES}',
            ID     : 'NotesFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'notes',
                Label  : '{i18n>ITEMS}',
                Target : 'notes/@UI.LineItem'
            }]
        },
        {
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>ORGANISATION}',
            ID     : 'OrganisationFacet',
            Facets : [{
                $Type  : 'UI.ReferenceFacet',
                ID     : 'SubSectionSalesAreaInformation',
                Label  : '{i18n>SALES_AREA_INFORMATION}',
                Target : '@UI.FieldGroup#SalesAreaInformation'
            }]
        },
        {
            $Type         : 'UI.CollectionFacet',
            Label         : '{i18n>ADDITIONAL_DETAILS}',
            ID            : 'AdditionalDetailsFacet',
            ![@UI.Hidden] : {$edmJson : {$Not : {$Path : 'individualComplaint'}}},
            Facets        : [{
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
    FilterFacets                           : [{
        $Type  : 'UI.ReferenceFacet',
        ID     : 'FilterFacetOther',
        Label  : '{i18n>OTHER}',
        Target : '@UI.FieldGroup#Other'
    }],
    FieldGroup #GeneralInformation         : {Data : [
        {
            $Type : 'UI.DataField',
            Value : complaintType_ID
        },
        {
            $Type : 'UI.DataField',
            Value : externalReference
        },
        {
            $Type : 'UI.DataField',
            Value : soldToParty_ID
        }
    ]},
    FieldGroup #ContactInformation         : {Data : [
        {
            $Type : 'UI.DataField',
            Value : createdBy
        },
        {
            $Type : 'UI.DataField',
            Value : modifiedBy
        }
    ]},
    FieldGroup #SingleComplaintMainDetails : {Data : [
        {
            $Type : 'UI.DataField',
            Value : soldToParty_ID
        },
        {
            $Type : 'UI.DataField',
            Value : item.material_ID,
            Label : '{i18n>COMPLAINT_MATERIAL}'
        },
        {
            $Type : 'UI.DataField',
            Value : item.materialUnknown
        },
        {
            $Type : 'UI.DataField',
            Value : item.complaintMaterial
        },
        {
            $Type : 'UI.DataField',
            Value : item.referenceQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : item.receivedQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : item.returnQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : item.complaintQuantity
        },
        {
            $Type : 'UI.DataField',
            Value : item.returnFollowupType_code,
            Label : '{i18n>RETURN_FOLLOW_UP_TYPE}'
        },
        {
            $Type : 'UI.DataField',
            Value : item.returnLocation
        },
        {
            $Type : 'UI.DataField',
            Value : externalReference
        },
        {
            $Type : 'UI.DataField',
            Value : item.confirmationStatus_code
        },
        {
            $Type : 'UI.DataField',
            Value : complaintChannel_ID
        }
    ]},
    FieldGroup #MultiComplaintMainDetails  : {Data : [
        {
            $Type : 'UI.DataField',
            Value : complaintReason_ID
        },
        {
            $Type : 'UI.DataField',
            Value : soldToParty_ID
        },
        {
            $Type : 'UI.DataField',
            Value : externalReference
        },
        {
            $Type : 'UI.DataField',
            Value : confirmationStatus_code
        }
    ]},
    FieldGroup #SalesAreaInformation       : {Data : [
        {
            $Type : 'UI.DataField',
            Value : salesOrganization_ID
        },
        {
            $Type : 'UI.DataField',
            Value : distributionChannel_ID
        },
        {
            $Type : 'UI.DataField',
            Value : division_ID
        }
    ]},
    FieldGroup #AdditionalDetails          : {Data : [
        {
            $Type : 'UI.DataField',
            Value : item.grossWeight
        },
        {
            $Type : 'UI.DataField',
            Value : item.netWeight
        },
        {
            $Type : 'UI.DataField',
            Value : item.weightUnit
        },
        {
            $Type : 'UI.DataField',
            Value : item.volume
        },
        {
            $Type : 'UI.DataField',
            Value : item.volumeUnit
        }
    ]},
    FieldGroup #InternalDetails            : {Data : [
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
    ]},
    FieldGroup #Other                      : {Data : [
        {
            $Type : 'UI.DataField',
            Value : externalReference
        },
        {
            $Type : 'UI.DataField',
            Value : salesOrganization_ID
        },
        {
            $Type : 'UI.DataField',
            Value : distributionChannel_ID
        },
        {
            $Type : 'UI.DataField',
            Value : division_ID
        },
        {
            $Type : 'UI.DataField',
            Value : complaintChannel_ID
        },
        {
            $Type : 'UI.DataField',
            Value : complaintCategory_code
        },
        {
            $Type : 'UI.DataField',
            Value : currency_code
        },
        {
            $Type : 'UI.DataField',
            Value : individualComplaint
        },
        {
            $Type : 'UI.DataField',
            Value : itemCategory_ID
        }
    ]},
    DataPoint #Status                      : {
        Value : confirmationStatus_code,
        Title : '{i18n>CONFIRMATION_STATUS}'
    }
});
