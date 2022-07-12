using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintReasonMappings with @(
    UI                            : {
        Identification :[
            {Value : ID},
             {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>ACTIVATE}',
                Action: 'ConfigurationService.ReactivateComplaintReasonMappings',
                ![@UI.Hidden]: isActive
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>DEACTIVATE}',
                Action: 'ConfigurationService.DeactivateComplaintReasonMappings',
                ![@UI.Hidden]: isInActive
            }
        ],
        HeaderInfo                  : {
            TypeName       : '{i18n>COMPLAINT_REASON_MAPPING}',
            TypeNamePlural : '{i18n>COMPLAINT_REASON_MAPPINGS}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>COMPLAINT_REASON}',
                Value : complaintReason.code
            },
            Description    : {
                $Type : 'UI.DataField',
                Label : '{i18n>ITEM_CATEGORY_DESCRIPTION}',
                Value : itemCategory.description
            },
        },
        Facets                      : [{
            $Type  : 'UI.CollectionFacet',
            Label  : '{i18n>GENERAL_INFORMATION}',
            ID     : 'GeneralInformationFacet',
            Facets : [
                {
                    $Type  : 'UI.ReferenceFacet',
                    ID     : 'SubSectionMainDetails',
                    Label  : '{i18n>MAIN_DETAILS}',
                    Target : '@UI.FieldGroup#MainDetails'
                },
                {
                    $Type  : 'UI.ReferenceFacet',
                    ID     : 'SubSectionAdminData',
                    Label  : '{i18n>INTERNAL_DETAILS}',
                    Target : '@UI.FieldGroup#InternalDetails'
                }
            ]
        }],
        FilterFacets                : [{
            $Type  : 'UI.ReferenceFacet',
            ID     : 'FilterFacetOther',
            Label  : '{i18n>OTHER}',
            Target : '@UI.FieldGroup#Other'
        }],
        FieldGroup #MainDetails     : {Data : [
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
                Value : itemCategory_ID
            },
            {
                $Type : 'UI.DataField',
                Value : complaintReason_ID
            }
        ]},
        FieldGroup #InternalDetails : {Data : [
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
        FieldGroup #Other           : {Data : [{
            $Type : 'UI.DataField',
            Value : identifier
        }]}
    },
    Common.SideEffects #salesArea : {
        SourceProperties : [salesOrganization_ID],
        TargetProperties : [
            'distributionChannel_ID',
            'division_ID'
        ],
        TargetEntities   : [
            salesOrganization,
            distributionChannel,
            division
        ]
    }
);
