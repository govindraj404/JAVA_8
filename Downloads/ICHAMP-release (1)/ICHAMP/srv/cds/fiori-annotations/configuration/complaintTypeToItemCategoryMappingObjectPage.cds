using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeToItemCategoryMappings with @(
    UI                            : {
        Identification :[
            {Value : ID},
             {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>ACTIVATE}',
                Action: 'ConfigurationService.ReactivateComplaintTypeToItemCategoryMappings',
                ![@UI.Hidden]: isActive
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>DEACTIVATE}',
                Action: 'ConfigurationService.DeactivateComplaintTypeToItemCategoryMappings',
                ![@UI.Hidden]: isInActive
            }
        ],
        HeaderInfo                  : {
            TypeName       : '{i18n>COMPLAINT_TYPE_ITEM_CATEGORY_MAPPING}',
            TypeNamePlural : '{i18n>COMPLAINT_TYPE_ITEM_CATEGORY_MAPPINGS}',
            Title          : {
                $Type : 'UI.DataField',
                Label : '{i18n>CODE}',
                Value : complaintType.code
            },
            Description    : {
                $Type : 'UI.DataField',
                Label : '{i18n>DESCRIPTION}',
                Value : complaintType.description
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
        FieldGroup #MainDetails     : {Data : [
            {
                $Type : 'UI.DataField',
                Value : complaintType_ID
            },
            {
                $Type : 'UI.DataField',
                Value : salesOrganization_ID
            },
            {
                $Type : 'UI.DataField',
                Value : distributionChannel_ID,
                Label : '{i18n>DISTRIBUTION_CHANNEL}'
            },
            {
                $Type : 'UI.DataField',
                Value : division_ID,
                Label : '{i18n>DIVISION}'
            },
            {
                $Type : 'UI.DataField',
                Value : itemCategory_ID
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
        ]}
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
    },
      Common.SideEffects #complaintType : {
        SourceProperties : [complaintType_ID],
        TargetProperties : [
            'complaintType.description'
        ],
        TargetEntities   : [
            complaintType
        ]
    }
);