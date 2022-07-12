package com.sap.ic.cmh.masterdata.materialmasterplantdata.handler;

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
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataService;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.service.MaterialMasterPlantDataService;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.validation.MaterialMasterPlantDataValidator;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas_;
import cds.gen.masterdataservice.Plants;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class MaterialMasterPlantDataHandler implements EventHandler {

    @Autowired
    MaterialMasterGeneralDataService materialMasterGeneralDataService;
    @Autowired
    MaterialMasterPlantDataService materialMasterPlantDataService;
    @Autowired
    PlantService plantService;
    /* MaterialMasterPlantDataValidator instance */
    @Autowired
    private MaterialMasterPlantDataValidator materialMasterPlantDataValidator;

    @Autowired
    private Messages messages;
    @Autowired
    @Qualifier("MasterDataService")
    private CdsService cdsService;

    public static final Logger logger =
            LoggerFactory.getLogger(MaterialMasterPlantDataHandler.class);

    @Before(event = CdsService.EVENT_CREATE, entity = MaterialMasterPlantDatas_.CDS_NAME)
    public void beforeMaterialMasterPlantDataCreate(CdsCreateEventContext context,
            MaterialMasterPlantDatas plantData) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(),"beforeMaterialMasterPlantDataCreate");
        /* input sanitization process */
        materialMasterPlantDataValidator.checkInputsSanitized(plantData);
        Plants plant =
                plantService.fetchPlant(plantData.getPlant(), MessageKeys.PLANT_DOES_NOT_EXIST,
                        MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::plant);

        if (plant != null) {
            plantData.setPlantIDId(plant.getId());
        }

        MaterialMasterGeneralDatas materialMasterGeneralData = materialMasterGeneralDataService
                .fetchMaterialMasterGeneralData(plantData.getMaterialCode(),
                        MessageKeys.MATERIAL_MASTER_GENERAL_DATA_DOES_NOT_EXIST,
                        MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::materialCode);

        if (materialMasterGeneralData != null) {
            plantData.setMaterialCodeIDId(materialMasterGeneralData.getId());
        }
        messages.throwIfError();

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(),"beforeMaterialMasterPlantDataCreate");
    }


    /**
     *
     * Method to create/update Material Master Plant Data
     * 
     * @param context context
     * @param plantData plantData
     */
    @On(event = CdsService.EVENT_CREATE, entity = MaterialMasterPlantDatas_.CDS_NAME)
    public void onMaterialMasterPlantDataCreate(CdsCreateEventContext context,
            MaterialMasterPlantDatas plantData) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(),
                "onMaterialMasterPlantDataCreate");
        MaterialMasterPlantDatas materialMasterPlantData = materialMasterPlantDataService
                .fetchMaterialMasterPlantDatas(plantData.getMaterialCode(), plantData.getPlant());
        if (null != materialMasterPlantData) {
            CqnUpdate update = Update.entity(MaterialMasterPlantDatas_.class).data(plantData);
            context.setResult(cdsService.run(update));
            context.setCompleted();

        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(),"onMaterialMasterPlantDataCreate");
    }

     /**	
     *  This method is used to update Material master general data records	
     * @param context	
     * @param materialMasterGeneralDataItem	
     */	
    @Before(event = CdsService.EVENT_UPDATE, entity = MaterialMasterPlantDatas_.CDS_NAME)	
    public void beforeUpdateMaterialMasterGeneralData(CdsUpdateEventContext context, MaterialMasterPlantDatas plantData) {	
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeUpdateMaterialMasterGeneralData");	
        logger.info("Inside Material Update .... ");
        /* input sanitization process */
        materialMasterPlantDataValidator.checkInputsSanitized(plantData);
        Plants plant =
                plantService.fetchPlant(plantData.getPlant(), MessageKeys.PLANT_DOES_NOT_EXIST,
                        MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::plant);

        if (plant != null) {
            plantData.setPlantIDId(plant.getId());
        }

        MaterialMasterGeneralDatas materialMasterGeneralData = materialMasterGeneralDataService
                .fetchMaterialMasterGeneralData(plantData.getMaterialCode(),
                        MessageKeys.MATERIAL_MASTER_GENERAL_DATA_DOES_NOT_EXIST,
                        MaterialMasterPlantDatas_.class, MaterialMasterPlantDatas_::materialCode);

        if (materialMasterGeneralData != null) {
            plantData.setMaterialCodeIDId(materialMasterGeneralData.getId());
        }
        messages.throwIfError();	
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeUpdateMaterialMasterGeneralData");	
    }
}
