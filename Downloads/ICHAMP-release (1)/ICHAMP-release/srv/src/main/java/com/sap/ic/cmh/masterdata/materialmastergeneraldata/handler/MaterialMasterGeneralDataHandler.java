package com.sap.ic.cmh.masterdata.materialmastergeneraldata.handler;

import org.slf4j.Logger;
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
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.validation.MaterialMasterGeneralDataValidator;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.MasterDataService_;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import cds.gen.masterdataservice.UnitOfMeasures;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class MaterialMasterGeneralDataHandler implements EventHandler {

    /* Message instance */
    @Autowired
    private Messages messages;

    /* MaterialMasterGeneralDataValidator instance */
    @Autowired
    private MaterialMasterGeneralDataValidator materialMasterGeneralDataValidator;
    @Autowired
    MaterialMasterGeneralDataService materialMasterGeneralDataService;
    @Autowired
    UnitOfMeasureService unitOfMeasureService;
    @Autowired
    @Qualifier(MasterDataService_.CDS_NAME)
	private CdsService cdsService;



    public static final Logger logger = LoggerHelper.getLogger(MaterialMasterGeneralDataHandler.class);


    @Before(event = CdsService.EVENT_CREATE, entity = MaterialMasterGeneralDatas_.CDS_NAME)
    public void beforeMaterialMasterGeneralDataCreate(CdsCreateEventContext context, MaterialMasterGeneralDatas materialMasterGeneralDataItem){
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeMaterialMasterGeneralDataCreate");
        validateUnitOfMeasure(materialMasterGeneralDataItem);
        /* input sanitization process */
        materialMasterGeneralDataValidator.checkInputsSanitized(materialMasterGeneralDataItem);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeMaterialMasterGeneralDataCreate");

    }

    /**
     *
     * Method used to create /update master data in database
     * @param context context
     * @param materialMasterGeneralDataItem context
     */
    @On(event = CdsService.EVENT_CREATE, entity = MaterialMasterGeneralDatas_.CDS_NAME)
    public void onMaterialMasterGeneralDataCreate(CdsCreateEventContext context, MaterialMasterGeneralDatas materialMasterGeneralDataItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onMaterialMasterGeneralDataCreate");
        /* Checking if provided master data item is present in database */
        MaterialMasterGeneralDatas materialData = materialMasterGeneralDataService.fetchMaterialMasterGeneralDataBasedOnCode(materialMasterGeneralDataItem.getMaterialCode());
        if(null!=materialData) {
        	String materialId = materialData.getId();
        	materialMasterGeneralDataItem.setId(materialId);
        	CqnUpdate update = Update.entity(MaterialMasterGeneralDatas_.class).data(materialMasterGeneralDataItem);
            context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onMaterialMasterGeneralDataCreate");
    }

	public void validateUnitOfMeasure(MaterialMasterGeneralDatas materialMasterGeneralDataItem) {
		UnitOfMeasures unitOfMeasure = unitOfMeasureService.getUnitOfMeasureDetails(materialMasterGeneralDataItem.getBaseUnitOfMeasureCode());
        if (null==unitOfMeasure) {
            messages.error(MessageKeys.INVALID_BASE_UNIT_OF_MEASURE).target("in", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::baseUnitOfMeasure_code);
        }
	}
    
    /**	
     *  This method is used to update Material master general data records	
     * @param context	
     * @param materialMasterGeneralDataItem	
     */	
    @Before(event = CdsService.EVENT_UPDATE, entity = MaterialMasterGeneralDatas_.CDS_NAME)	
    public void beforeUpdateMaterialMasterGeneralData(CdsUpdateEventContext context, MaterialMasterGeneralDatas materialMasterGeneralDataItem) {	
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeUpdateMaterialMasterGeneralData");	
        logger.info("Inside Material Update .... ");
        validateUnitOfMeasure(materialMasterGeneralDataItem);
        materialMasterGeneralDataValidator.checkInputsSanitized(materialMasterGeneralDataItem);
        messages.throwIfError();	
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeUpdateMaterialMasterGeneralData");	
    }
}