namespace com.sap.ic.cmh.salesArea;

using {com.sap.ic.cmh.salesOrganization.SalesOrganizations} from './index';
using {com.sap.ic.cmh.division.Divisions} from './index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannels} from './index';

entity SalesAreas as

    select from DistributionChannels
    left outer join Divisions
        on DistributionChannels.salesOrganizationID.ID = Divisions.salesOrganizationID.ID
    {
        key DistributionChannels.ID                                        as distributionChannelID,
        key DistributionChannels.salesOrganizationID                       as salesOrganizationID,
        key Divisions.ID                                                   as divisionID,
            DistributionChannels.distributionChannelName,
            DistributionChannels.distributionChannel,
            Divisions.salesDivision                                        as salesDivision,
            Divisions.salesDivisionName                                    as salesDivisionName,
            DistributionChannels.salesOrganizationID.salesOrganization     as salesOrganization,
            DistributionChannels.salesOrganizationID.salesOrganizationName as salesOrganizationName
    };
