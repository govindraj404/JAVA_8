using ConfigurationService from '../../services/index';

annotate ConfigurationService.ComplaintTypeToSalesAreaMappings {

     ID
                       @UI.Hidden;

     salesOrganization_ID
                       @Common : {

          Label        : '{i18n>SALES_ORGANIZATION}',
          FieldControl : #Mandatory
     };


     salesOrganization @Common : {
          Text             : salesOrganization.salesOrganizationName,
          TextArrangement  : #TextOnly,
          Label            : '{i18n>SALES_ORGANIZATION}',
          FieldControl     : #Mandatory,
          ValueListMapping : {
               Label          : '{i18n>SALES_ORGANIZATION}',
               CollectionPath : 'SalesAreas',
               Parameters     : [
                    {
                         $Type             : 'Common.ValueListParameterInOut',
                         ValueListProperty : 'salesOrganizationID_ID',
                         LocalDataProperty : salesOrganization_ID
                    },
                    {
                         $Type             : 'Common.ValueListParameterInOut',
                         ValueListProperty : 'distributionChannelID',
                         LocalDataProperty : distributionChannel_ID
                    },
                    {
                         $Type             : 'Common.ValueListParameterInOut',
                         ValueListProperty : 'divisionID',
                         LocalDataProperty : division_ID
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'salesOrganization',
                         ![@UI.Importance] : #High
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'salesOrganizationName',
                         ![@UI.Importance] : #High
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'distributionChannel',
                         ![@UI.Importance] : #High
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'distributionChannelName',
                         ![@UI.Importance] : #High
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'salesDivision',
                         ![@UI.Importance] : #High
                    },
                    {
                         $Type             : 'Common.ValueListParameterDisplayOnly',
                         ValueListProperty : 'salesDivisionName',
                         ![@UI.Importance] : #High
                    }
               ]
          }
     };

     distributionChannel_ID
                       @Common : {
          Text            : distributionChannel.distributionChannelName,
          TextArrangement : #TextOnly,
          Label           : '{i18n>DISTRIBUTION_CHANNEL}',
          FieldControl    : #Mandatory
     };


     distributionChannel
                       @Common : {
          Text            : distributionChannel.distributionChannelName,
          TextArrangement : #TextOnly,
          Label           : '{i18n>DISTRIBUTION_CHANNEL}',
          FieldControl    : #Mandatory
     };


     division_ID
                       @Common : {
          Label        : '{i18n>DIVISION}',
          FieldControl : #Mandatory
     };


     division
                       @Common : {
          Text            : division.salesDivisionName,
          TextArrangement : #TextOnly,
          Label           : '{i18n>DIVISION}',
          FieldControl    : #Mandatory
     };

     complaintTypeConfiguration
                       @UI.Hidden;

}


annotate ConfigurationService.SalesOrganizations {
     salesOrganizationName
     @Common : {FieldControl : #ReadOnly};
}


annotate ConfigurationService.DistributionChannels {
     distributionChannelName
     @Common : {FieldControl : #ReadOnly};
}

annotate ConfigurationService.Divisions {
     salesDivisionName
     @Common : {FieldControl : #ReadOnly};
}
