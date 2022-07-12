using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.distributionChannel.DistributionChannels with {

    ID
    @UI.Hidden    : true
    @Common       : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : distributionChannelName,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    distributionChannel
    @Common.Label : '{i18n>DISTRIBUTION_CHANNEL}';

    distributionChannelName
    @Common.Label : '{i18n>DESCRIPTION}';

    salesOrganizationID
    @UI.HiddenFilter
    @UI.Hidden;

    salesOrganizationID_ID
    @UI.Hidden;

}

annotate cmh.distributionChannel.DistributionChannels with @(Capabilities.SearchRestrictions.Searchable : true);


annotate cmh.distributionChannel.DistributionChannel with
@(Common.ValueListMapping : {
    Label          : '{i18n>DISTRIBUTION_CHANNEL}',
    CollectionPath : 'DistributionChannels',
    Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'ID',
            LocalDataProperty : distributionChannel_ID
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
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'salesOrganizationID_ID',
            LocalDataProperty : salesOrganization_ID
        }
    ]
});
