using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintChannels with @(UI : {
    Identification :[
            {Value : ID},
             {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>ACTIVATE}',
                Action: 'ConfigurationService.ReactivateComplaintChannels',
                ![@UI.Hidden]: isActive
            },
            {
                $Type  : 'UI.DataFieldForAction',
                Label  : '{i18n>DEACTIVATE}',
                Action: 'ConfigurationService.DeactivateComplaintChannels',
                ![@UI.Hidden]: isInActive
            }
        ],
    HeaderInfo                  : {
        TypeName       : '{i18n>COMPLAINT_CHANNEL}',
        TypeNamePlural : '{i18n>COMPLAINT_CHANNELS}',
        Title          : {
            $Type : 'UI.DataField',
            Label : '{i18n>CODE}',
            Value : code
        },
        Description    : {
            $Type : 'UI.DataField',
            Label : '{i18n>DESCRIPTION}',
            Value : description
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
            Value : code
        },
        {
            $Type : 'UI.DataField',
            Value : description
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
});
