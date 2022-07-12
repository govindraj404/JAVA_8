package com.sap.ic.cmh.masterdata.plant.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import com.sap.ic.cmh.masterdata.plant.validation.PlantValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class PlantHandler implements EventHandler {

    @Autowired
    private AddressService addressService;

    @Autowired
	CompanyCodeService companyCodeService;

    /* PlantValidator instance */
    @Autowired
    private PlantValidator plantValidator;
    
    @Autowired
    Messages messages;
    @Autowired
    PlantService plantService;
    
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;


    public static final Logger logger = LoggerFactory.getLogger(PlantHandler.class);
    
    /**
     *  This method is used to perform Business validation
     *   with Address & CompanyCode for create
     * @param context
     * @param plantItem
     */
    @Before(event = CdsService.EVENT_CREATE, entity = Plants_.CDS_NAME)
    public void beforePlantsCreate(CdsCreateEventContext context, Plants plantItem){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforePlantsCreate");

        plantValidator.checkInputsSanitized(plantItem);
        setAddressId(plantItem);
        setCompanyId(plantItem);
        messages.throwIfError();

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforePlantsCreate");
    }


    /**
     *  This method is used to Create plant Details based on the Business validation
     *   with Address & CompanyCode
     * @param context
     * @param plantItem
     */
    @On(event = CdsService.EVENT_CREATE, entity = Plants_.CDS_NAME)
    public void onPlantsCreate(CdsCreateEventContext context, Plants plantItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onPlantsCreate");
        
        Plants plantDetails = plantService.fetchPlantDetailsBasedOnNumber(plantItem.getPlant());
        if(null!=plantDetails) {
        	String plantId = plantDetails.getId();
        	plantItem.setId(plantId);
        	CqnUpdate update = Update.entity(Plants_.class).data(plantItem);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onPlantsCreate");
    }
    
     /**
     * This method is used to perform Business validation
     *  with Address & CompanyCode for update
     * @param context
     * @param plantItem
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = Plants_.CDS_NAME)	
    public void beforePlantsUpdate(CdsUpdateEventContext context,  Plants plantItem) {	
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforePlantsUpdate");	
        plantValidator.checkInputsSanitized(plantItem);
        setAddressId(plantItem);
        setCompanyId(plantItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforePlantsUpdate");	
    }
    
    /**
     * Fetch company id based on company code
     * and set it to plant
     * @param plantItem
     */
	public void setCompanyId(Plants plantItem) {
		CompanyCodes companyCode = companyCodeService.fetchCompanyCode(plantItem.getCompanyCode(),
                MessageKeys.COMPANY_CODE_DOES_NOT_EXIST, Plants_.class, Plants_::companyCode);
        if (companyCode != null) {
            plantItem.setCompanyCodeIDId(companyCode.getId());
        }
	}
    
	/**
	 * Fetch address id based on address
     * and set it to plant
	 * @param plantItem
	 */
	public void setAddressId(Plants plantItem) {
		Addresses address = addressService.fetchAddress(plantItem.getAddress(),
                MessageKeys.ADDRESS_DOES_NOT_EXIST, Plants_.class, Plants_::address);
        if (address != null) {
            plantItem.setAddressIDId(address.getId());
        }
	}

}
