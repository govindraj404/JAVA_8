package com.sap.ic.cmh.masterdata.distributionchannel.validation;

import cds.gen.masterdataservice.DistributionChannels;

public interface DistributionChannelValidation {

    void checkInputsSanitized(DistributionChannels distributionChannel);
}
