package com.sap.ic.cmh.masterdata.defectcode.handler;

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
import com.sap.ic.cmh.masterdata.defectcode.service.DefectCodeService;
import com.sap.ic.cmh.masterdata.defectcode.validation.DefectCodeValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectCodes_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class DefectCodesHandler implements EventHandler {

    /* DefectCodeValidator instance */
    @Autowired
    private DefectCodeValidator defectCodeValidator;

    @Autowired
    Messages messages;
    @Autowired
    DefectCodeService defectCodeService;
    @Autowired
    @Qualifier("MasterDataService")
    private CdsService cdsService;

    public static final Logger logger = LoggerFactory.getLogger(DefectCodesHandler.class);

    /**
     * This method is used to perform business validation
     * 
     * @param context     cdsCreateContext
     * @param defectCodes provided addressItem
     */
    @Before(event = CdsService.EVENT_CREATE, entity = DefectCodes_.CDS_NAME)
    public void beforeDefectCodeCreate(CdsCreateEventContext context, DefectCodes defectCodes) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeDefectCodeCreate");
        defectCodeValidator.checkInputsSanitized(defectCodes);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeDefectCodeCreate");
    }

    /**
     * This method is used to Create defect code records
     * If defect code exists, update the existing record
     * 
     * @param context     cdsCreateContext
     * @param defectCodes provided addressItem
     */
    @On(event = CdsService.EVENT_CREATE, entity = DefectCodes_.CDS_NAME)
    public void onDefectCodeCreate(CdsCreateEventContext context, DefectCodes defectCodes) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onDefectCodeCreate");
        DefectCodes defectCodeDetails = defectCodeService.fetchDefectCode(defectCodes.getCode(),
                defectCodes.getDefectGroupCode());
        if (null != defectCodeDetails) {
            CqnUpdate update = Update.entity(DefectCodes_.class).data(defectCodes);
            context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onDefectCodeCreate");
    }

    /**
     * updateDefectCode
     * 
     * @param context
     * @param defectCodes
     */

    @Before(event = CdsService.EVENT_UPDATE, entity = DefectCodes_.CDS_NAME)
    public void updateDefectCode(CdsUpdateEventContext context, DefectCodes defectCodes) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "updateDefectCode");
        defectCodeValidator.checkInputsSanitized(defectCodes);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "updateDefectCode");
    }
}