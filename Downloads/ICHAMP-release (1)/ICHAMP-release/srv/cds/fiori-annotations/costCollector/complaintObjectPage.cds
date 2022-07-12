using CostCollectorService from '../../services/index';

annotate CostCollectorService.Complaints with @(
  UI : {
    HeaderInfo : {
      TypeName       : '{i18n>COST_COLLECTOR}',
      TypeNamePlural : '{i18n>COMPLAINTS}',
      Title          : {
        $Type : 'UI.DataField',
        Value : '{i18n>COST_COLLECTOR}'
      },
      Description    : {
        $Type : 'UI.DataField',
        Value : '{i18n>COST_COLLECTOR}'
      }
    },
    HeaderFacets : 
    [
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>TOTAL_SUBLET_COST}',
        Target : '@UI.DataPoint#TotalCost'
      },
      {
        $Type  : 'UI.ReferenceFacet',
        Label  : '{i18n>TOTAL_LABOR}',
        Target : '@UI.DataPoint#TotalLabor'
      }
    ],
    Facets : 
    [
      {
        $Type  : 'UI.CollectionFacet',
        ID     : 'ReferenceInformationFacet',
        Label  : '{i18n>COST}',
        Facets : 
        [
          {
            $Type  : 'UI.ReferenceFacet',
            Label  : '{i18n>ITEMS}',
            ID     : 'SubSectionReferenceDetails',
            Target : 'costCollector/@UI.LineItem'
          }
        ]
      }
    ],
    DataPoint #TotalCost : {
      Value : totalSubLetCost,
      Title : '{i18n>TOTAL_SUBLET_COST}'
    },
    DataPoint #TotalLabor      : {
      Value : totalLaborHour,
      Title : '{i18n>TOTAL_LABOR}'
    }
  }
);