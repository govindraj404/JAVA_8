package com.sap.ic.cmh.masterdata.distributionchannel.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.Struct;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.distributionchannel.service.DistributionChannelService;
import com.sap.ic.cmh.masterdata.distributionchannel.validation.DistributionChannelValidation;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.DistributionChannels_;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * DistributionChannelHandler used to handle Distribution Channel event
 */
@Component
@RequestScope
@ServiceName ("MasterDataService")
public class DistributionChannelHandler implements EventHandler {


    public static final Logger logger = LoggerFactory.getLogger(DistributionChannelHandler.class);
    @Autowired
    SalesOrganizationService salesOrganizationService;
    @Autowired
    DistributionChannelService distributionChannelService;
    /* Message instance */
    @Autowired
    private Messages messages;
    /* DistributionChannelValidator instance */
    @Autowired
    private DistributionChannelValidation distributionChannelValidator;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    /**
     * Method to do sanitization before processing distribution channel for Create
     *
     * @param distributionChannelPayload
     */
    @Before (event = CdsService.EVENT_CREATE, entity = DistributionChannels_.CDS_NAME)
    public void beforeDistributionChannelsCreate(DistributionChannels distributionChannelPayload) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeDistributionChannelsCreate");
        messages.info(distributionChannelPayload.toJson(), distributionChannelPayload);
        distributionChannelValidator.checkInputsSanitized(distributionChannelPayload);
        fetchSalesOrganization(distributionChannelPayload);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeDistributionChannelsCreate");
    }


	/**
	 * Method used to create /update distribution channel
	 *
	 * @param context                 context
	 * @param distributionChannelPayload distributionChannelItem
	 */
	@On(event = CdsService.EVENT_CREATE, entity = DistributionChannels_.CDS_NAME)
	public void onDistributionChannelsCreate(CdsCreateEventContext context, DistributionChannels distributionChannelPayload) {

		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onDistributionChannelsCreate");
		messages.info(distributionChannelPayload.toJson(), distributionChannelPayload);

		DistributionChannels distributionChannelsDetailsFromDb = distributionChannelService.getDetailsBasedOnDistributionChannelAndSalesOrg(
				distributionChannelPayload.getDistributionChannel(), distributionChannelPayload.getSalesOrganization());
		if(null!=distributionChannelsDetailsFromDb) {
			//Handled patch
			DistributionChannels distributionChannelsToBeUpdated = Struct.create(DistributionChannels.class);
			distributionChannelsToBeUpdated.putAll(distributionChannelPayload);
			distributionChannelPayload.putAll(distributionChannelsDetailsFromDb);
			distributionChannelPayload.putAll(distributionChannelsToBeUpdated);
			distributionChannelPayload.setId(distributionChannelsDetailsFromDb.getId());
			CqnUpdate update = Update.entity(DistributionChannels_.class).data(distributionChannelPayload);
            context.setResult(cdsService.run(update));
            context.setCompleted();
		}

		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onDistributionChannelsCreate");
	}
    
    /**
     * Method to do sanitization before processing distribution channel for Create
     *
     * @param distributionChannelPayload
     */
    @Before (event = CdsService.EVENT_UPDATE, entity = DistributionChannels_.CDS_NAME)
    public void beforeDistributionChannelsUpdate(DistributionChannels distributionChannelPayload) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeDistributionChannelsUpdate");
        
        messages.info(distributionChannelPayload.toJson(), distributionChannelPayload);
        distributionChannelValidator.checkInputsSanitized(distributionChannelPayload);
        DistributionChannels distributionChannelsDetailsFromDb = distributionChannelService.getDetailsBasedOnDistributionChannelAndSalesOrg(
        		distributionChannelPayload.getDistributionChannel(), distributionChannelPayload.getSalesOrganization());
		if(null!=distributionChannelsDetailsFromDb) {
			//Handled patch
			DistributionChannels distributionChannelsToBeUpdated = Struct.create(DistributionChannels.class);
			distributionChannelsToBeUpdated.putAll(distributionChannelPayload);
			distributionChannelPayload.putAll(distributionChannelsDetailsFromDb);
			distributionChannelPayload.putAll(distributionChannelsToBeUpdated);
		}
		fetchSalesOrganization(distributionChannelPayload);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeDistributionChannelsUpdate");
    }
    
    /**
     * Fetch Sales Org Data
     * @param distributionChannelPayload
     */
	public void fetchSalesOrganization(DistributionChannels distributionChannelPayload) {
		SalesOrganizations salesOrganization = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(distributionChannelPayload.getSalesOrganization());
        if (salesOrganization != null) {
            distributionChannelPayload.setSalesOrganizationIDId(salesOrganization.getId());
        }else {
        	messages.error(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST).target("in", DistributionChannels_.class,
        			DistributionChannels_::salesOrganization);
        }
	}

}
