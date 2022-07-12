package com.sap.ic.cmh.masterdata.division.handler;

import org.slf4j.Logger;
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
import com.sap.ic.cmh.masterdata.division.service.DivisionService;
import com.sap.ic.cmh.masterdata.division.validation.DivisionValidation;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Divisions;
import cds.gen.masterdataservice.Divisions_;
import cds.gen.masterdataservice.SalesOrganizations;

/**
 * DivisionHandler class  used to handle Division event
 */
@Component
@RequestScope
@ServiceName ("MasterDataService")
public class DivisionHandler implements EventHandler {

    public static final Logger logger = LoggerHelper.getLogger(DivisionHandler.class);
    @Autowired
    SalesOrganizationService salesOrganizationService;
    @Autowired
    DivisionService divisionService;
    /* Messages instance */
    @Autowired
    private Messages messages;
    /* DivisionValidator instance */
    @Autowired
    private DivisionValidation divisionValidator;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    /**
     * Method to do sanitization before processing division for Create
     *
     * @param divisionItem
     */
    @Before (event = CdsService.EVENT_CREATE, entity = Divisions_.CDS_NAME)
    public void beforeDivisionsCreate(Divisions divisionItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeDivisionsCreate");
        messages.info(divisionItem.toJson(), divisionItem);
        divisionValidator.checkInputsSanitized(divisionItem);
        fetchSalesOrganization(divisionItem);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeDivisionsCreate");
    }

 

	/**
	 * Method used to create /update division
	 *
	 * @param context
	 * @param divisionItem
	 */
	@On(event = CdsService.EVENT_CREATE, entity = Divisions_.CDS_NAME)
	public void onDivisionsCreate(CdsCreateEventContext context, Divisions divisionItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "createDivision");
		Divisions divisionDetailsFromDb = divisionService
				.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisionItem.getSalesDivision(),
						divisionItem.getSalesOrganization());
		if(null!=divisionDetailsFromDb) {
			//Handled Patch operation
			Divisions divisionsToBeUpdated = Struct.create(Divisions.class);
			divisionsToBeUpdated.putAll(divisionItem);
			divisionItem.putAll(divisionDetailsFromDb);
			divisionItem.putAll(divisionsToBeUpdated);
			divisionItem.setId(divisionDetailsFromDb.getId());
			CqnUpdate update = Update.entity(Divisions_.class).data(divisionItem);
            context.setResult(cdsService.run(update));
            context.setCompleted();
		}
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "createDivision");
	}
	
	/**
     * Method to do sanitization before processing division for Update
     *
     * @param divisionItem
     */
    @Before (event = CdsService.EVENT_UPDATE, entity = Divisions_.CDS_NAME)
    public void beforDivisionsUpdate(Divisions divisionItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforDivisionsUpdate");
        messages.info(divisionItem.toJson(), divisionItem);
        //Handled Patch operation
        Divisions divisionDetailsFromDb = divisionService
				.getDivisionDetailsBasedOnDivisionAndSalesOrg(divisionItem.getSalesDivision(),
						divisionItem.getSalesOrganization());
		if(null!=divisionDetailsFromDb) {
			
			Divisions divisionsToBeUpdated = Struct.create(Divisions.class);
			divisionsToBeUpdated.putAll(divisionItem);
			divisionItem.putAll(divisionDetailsFromDb);
			divisionItem.putAll(divisionsToBeUpdated);
		}
        divisionValidator.checkInputsSanitized(divisionItem);
        fetchSalesOrganization(divisionItem);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforDivisionsUpdate");
    }
    
    /**
     * fetch Sales Org detail
     * @param divisionItem
     */
	public void fetchSalesOrganization(Divisions divisionItem) {
		SalesOrganizations salesOrganization = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(divisionItem.getSalesOrganization());
        if (salesOrganization != null) {
            divisionItem.setSalesOrganizationIDId(salesOrganization.getId());
        }else {
        	messages.error(MessageKeys.SALES_ORGANIZATION_DOES_NOT_EXIST).target("in", Divisions_.class,
					Divisions_::salesOrganization);
        }
	}

}