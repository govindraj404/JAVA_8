using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.salesArea.SalesAreas with {

    salesOrganizationID
    @UI.Hidden;

    distributionChannelName
    @UI.HiddenFilter;

    salesDivisionName
    @UI.HiddenFilter;
}
