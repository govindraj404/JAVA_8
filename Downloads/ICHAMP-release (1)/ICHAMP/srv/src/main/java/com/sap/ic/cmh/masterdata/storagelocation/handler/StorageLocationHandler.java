package com.sap.ic.cmh.masterdata.storagelocation.handler;

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
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import com.sap.ic.cmh.masterdata.storagelocation.service.StorageLocationService;
import com.sap.ic.cmh.masterdata.storagelocation.validation.StorageLocationValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.StorageLocations;
import cds.gen.masterdataservice.StorageLocations_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class StorageLocationHandler implements EventHandler {

    public static final Logger logger = LoggerFactory.getLogger(StorageLocationHandler.class);

    @Autowired
    PlantService plantService;

    @Autowired
    private StorageLocationValidator storageLocationValidator;

    @Autowired
    Messages messages;
    @Autowired
    StorageLocationService storageLocationService;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;
    

    /**
     * This method is to perform Business Validation for create
     * @param context
     * @param storageLocation
     */
    @Before(event = CdsService.EVENT_CREATE, entity = StorageLocations_.CDS_NAME)
    public void beforeCreateStorageLocation(CdsCreateEventContext context, StorageLocations storageLocation){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeCreateStorageLocation");
        storageLocationValidator.checkInputsSanitized(storageLocation);
        Plants plant = plantService.fetchPlant(storageLocation.getPlantCode(),
                MessageKeys.PLANT_DOES_NOT_EXIST, StorageLocations_.class, StorageLocations_::plantCode);

        if (plant != null) {
            storageLocation.setPlantCodeIDId(plant.getId());
        }
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeCreateStorageLocation");
    }

    /**
     * This method is used to create /update storage location detail
     * @param context
     * @param storageLocation
     */
    @On(event = CdsService.EVENT_CREATE, entity = StorageLocations_.CDS_NAME)
    public void onCreateStorageLocation(CdsCreateEventContext context, StorageLocations storageLocation) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onCreateStorageLocation");
        StorageLocations storageLocationDetails=  storageLocationService.fetchStorageLocations(storageLocation.getStorageLocation(), storageLocation.getPlantCodeIDId());
        if(null!=storageLocationDetails){
            CqnUpdate update = Update.entity(StorageLocations_.class).data(storageLocation);
        	context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onCreateStorageLocation");
    }
    

    /**
     * This method is to perform Business Validation for update
     * @param context
     * @param storageLocation
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = StorageLocations_.CDS_NAME)
    public void beforeStorageLocationUpdate(CdsUpdateEventContext context, StorageLocations storageLocation) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeStorageLocationUpdate");
        storageLocationValidator.checkInputsSanitized(storageLocation);
        Plants plant = plantService.fetchPlant(storageLocation.getPlantCode(),
                MessageKeys.PLANT_DOES_NOT_EXIST, StorageLocations_.class, StorageLocations_::plantCode);

        if (plant != null) {
            storageLocation.setPlantCodeIDId(plant.getId());
        }
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeStorageLocationUpdate");
    }
}
