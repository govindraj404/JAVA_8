package com.sap.ic.cmh.masterdata.distributionchannel.validation;


import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.DistributionChannels_;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * This class validates the distribution channel details
 */
@Component
public class DistributionChannelValidationImpl implements DistributionChannelValidation {

    public static final Logger logger = LoggerHelper.getLogger(DistributionChannelValidationImpl.class);

    @Autowired
    private DataValidator dataValidator;
    @Autowired
    Messages messages;
    @Autowired
    SalesOrganizationService salesOrganizationService;

    /**
     * Method to validate the distribution channel input details
     * @param distributionChannel
     */
    @Override
    public void checkInputsSanitized(DistributionChannels distributionChannel) {

        dataValidator.validateData(distributionChannel.getDistributionChannel(), MessageKeys.DISTRIBUTION_CHANNEL_DOES_NOT_EXIST, DistributionChannels_.class, DistributionChannels_::distributionChannel);
        dataValidator.validateData(distributionChannel.getDistributionChannelName(), MessageKeys.DISTRIBUTION_CHANNEL_NAME_VALIDATION_ERROR, DistributionChannels_.class, DistributionChannels_::distributionChannelName);
        dataValidator.validateData(distributionChannel.getSalesOrganization(), MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST, DistributionChannels_.class, DistributionChannels_::salesOrganization);
        validateSalesOrganization(distributionChannel.getSalesOrganization());
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }

    /**
     * Validate Sales Organization
     * @param salesOrganization
     */
	public void validateSalesOrganization(String salesOrganization) {
		SalesOrganizations salesOrganizationDetails = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganization);
		if(null==salesOrganizationDetails) {
			messages.error(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST).target("in", DistributionChannels_.class,
					DistributionChannels_::salesOrganization);
		}
	}
}
