using ComplaintService from '../../services/index';

annotate ComplaintService.Complaints with @(
  UI                                : {
    UpdateHidden                   : isUpdateRestricted,
    HeaderInfo                     : {
      TypeName       : '{i18n>SUPPLIER_COMPLAINT}',
      TypeNamePlural : '{i18n>COMPLAINTS}',
      Title          : {
        $Type : 'UI.DataField',
        Value : identifier

      },
      Description    : {
        $Type : 'UI.DataField',
        Value : '{i18n>SUPPLIER_RECOVERY}'
      }
    },
    Identification                 : [
      {
        $Type         : 'UI.DataFieldForAction',
        Label         : '{i18n>DISCARD}',
        Action        : 'ComplaintService.Discard',
        ![@UI.Hidden] : isHideDiscardComplaint
      },
      {
        $Type         : 'UI.DataFieldForAction',
        Label         : '{i18n>CLOSE}',
        Action        : 'ComplaintService.Close',
        ![@UI.Hidden] : isHideCloseComplaint
      },
      {
        $Type         : 'UI.DataFieldForAction',
        Label         : '{i18n>REOPEN}',
        Action        : 'ComplaintService.Reopen',
        ![@UI.Hidden] : isHideReopenComplaint
      }
    ],
    // Identification                 : [{
    // $Type          : 'UI.DataFieldForIntentBasedNavigation',
    // Label          : '{i18n>COST_COLLECTOR}',
    // Action         : 'manage',
    // ![@UI.Hidden]  : isHideCostCollection,
    // SemanticObject : 'costcollector'
    // }],

    HeaderFacets                   : [

      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>GENERAL_INFORMATION}',
        Target : '@UI.FieldGroup#HeaderDetails',
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>CONTACT_INFORMATION}',
        Target : '@UI.FieldGroup#HeaderFacetDetails',
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>DESCRIPTION}',
        Target : '@UI.FieldGroup#Description',
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Target : '@UI.DataPoint#Status',
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>TOTAL_SUBLET_COST}',
        Target : '@UI.DataPoint#TotalCost'
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>TOTAL_LABOR}',
        Target : '@UI.DataPoint#TotalHour'
      }
    ],
    Facets                         : [
      {
        $Type         : 'UI.CollectionFacet',
        Label         : '{i18n>HEADER}',
        ID            : 'HeaderInformationFacet',
        ![@UI.Hidden] : IsActiveEntity,
        Facets        : [
          {
            $Type  : 'UI.ReferenceFacet',
            ID     : 'SubSectionHeaderDetails',
            Target : '@UI.FieldGroup#HeaderDetails'
          },
          {
            $Type  : 'UI.ReferenceFacet',
            ID     : 'SubSectionReferenceDetails',
            Target : '@UI.FieldGroup#ReferenceDetails'
          }
        ]

      },
      {
        $Type  : 'UI.CollectionFacet',
        Label  : '{i18n>GENERAL_INFORMATION}',
        ID     : 'GeneralInformationFacet',
        Facets : [
          {
            $Type  : 'UI.ReferenceFacet',
            ID     : 'SubSectionComplaintDetails',
            Label  : '{i18n>COMPLAINT_DETAILS}',
            Target : '@UI.FieldGroup#ComplaintDetails'
          },
          {
            $Type  : 'UI.ReferenceFacet',
            ID     : 'SubSectionInternalDetails',
            Label  : '{i18n>INTERNAL_DETAILS}',
            Target : '@UI.FieldGroup#InternalDetails'
          },
        ]
      },
      {
        $Type         : 'UI.CollectionFacet',
        Label         : '{i18n>STREAMS}',
        ID            : 'ReferenceInformationFacet',
        ![@UI.Hidden] : isShowStreams,
        Facets        : [{
          $Type  : 'UI.ReferenceFacet',
          ID     : 'streamFacet',
          Label  : '{i18n>ITEMS}',
          Target : 'businessObjects/@UI.LineItem'
        }]
      },
      {
        $Type         : 'UI.CollectionFacet',
        Label         : '{i18n>STREAMS}',
        ID            : 'ReferenceInformationDraftFacet',
        ![@UI.Hidden] : IsActiveEntity,
        Facets        : [{
          $Type  : 'UI.ReferenceFacet',
          ID     : 'streamDraftFacet',
          Label  : '{i18n>ITEMS}',
          Target : 'draftBusinessObjects/@UI.LineItem'
        }]
      }
    ],
    FilterFacets                   : [{
      $Type  : 'UI.ReferenceFacet',
      ID     : 'FilterFacetOther',
      Label  : '{i18n>OTHER}',
      Target : '@UI.FieldGroup#Other'
    }],
    FieldGroup #ComplaintDetails   : {Data : [
      {
        $Type : 'UI.DataField',
        Value : material_ID
      },
      {
        $Type : 'UI.DataField',
        Value : quantity
      },
      {
        $Type : 'UI.DataField',
        Value : note
      }
    ]},
    FieldGroup #InternalDetails    : {Data : [
      {
        $Type : 'UI.DataField',
        Value : plant_ID
      },
      {
        $Type : 'UI.DataField',
        Value : companyCode_ID
      },
      {
        $Type : 'UI.DataField',
        Value : purchasingOrganization_ID
      },
      {
        $Type : 'UI.DataField',
        Value : createdBy
      },
      {
        $Type : 'UI.DataField',
        Value : createdAt,
        Label : '{i18n>COMPLAINT_DATE}'
      },
      {
        $Type : 'UI.DataField',
        Value : creationType
      }
    ]},

    FieldGroup #HeaderDetails      : {Data : [
      {
        $Type : 'UI.DataField',
        Value : referenceNumber
      },
      {
        $Type : 'UI.DataField',
        Value : supplier_ID
      }
    ]},

    FieldGroup #ReferenceDetails   : {Data : [
      {
        $Type : 'UI.DataField',
        Value : personResponsible_ID
      },
      {
        $Type : 'UI.DataField',
        Value : contactPerson_ID
      },
      {
        $Type : 'UI.DataField',
        Value : description
      }
    ]},

    FieldGroup #HeaderFacetDetails : {Data : [
      {
        $Type : 'UI.DataField',
        Value : personResponsible_ID
      },
      {
        $Type : 'UI.DataField',
        Value : contactPerson_ID
      }
    ]},
    FieldGroup #Description        : {Data : [{
      $Type : 'UI.DataField',
      Value : description
    }]},
    FieldGroup #Other              : {Data : [
      {
        $Type : 'UI.DataField',
        Value : quantity
      },
      {
        $Type : 'UI.DataField',
        Value : modifiedAt
      },
      {
        $Type : 'UI.DataField',
        Value : modifiedBy
      },
      {
        $Type : 'UI.DataField',
        Value : currency_code
      },
      {
        $Type : 'UI.DataField',
        Value : totalLaborHour
      },
      {
        $Type : 'UI.DataField',
        Value : totalSubLetCost
      },
      {
        $Type : 'UI.DataField',
        Value : createdBy
      },
      {
        $Type : 'UI.DataField',
        Value : purchasingOrganization_ID
      }
    ]},
    DataPoint #Status              : {
      Value : complaintStatus_code,
      Title : '{i18n>STATUS}'
    },
    DataPoint #TotalCost           : {
      Value : totalSubLetCost,
      Title : '{i18n>TOTAL_SUBLET_COST}'
    },
    DataPoint #TotalHour           : {
      Value : totalLaborHour,
      Title : '{i18n>TOTAL_LABOR}'
    }
  },
  Common.SideEffects #complaintPage : {
    SourceProperties : [plant_ID],
    TargetProperties : [
      'companyCode_ID',
      'currency_code'
    ],
    TargetEntities   : [CompanyCodes]
  }
);
