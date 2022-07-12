package com.sap.ic.cmh.masterdata.defectgroup.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import java.util.Map;
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
import com.sap.ic.cmh.masterdata.defectgroup.service.DefectGroupService;
import com.sap.ic.cmh.masterdata.defectgroup.validation.DefectGroupValidator;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.DefectGroups_;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class DefectGroupHandler implements EventHandler {

    /* DefectGroupValidator instance */
    @Autowired
    private DefectGroupValidator defectGroupValidator;

    @Autowired
    Messages messages;
    @Autowired
    DefectGroupService defectGroupService;
    @Autowired
    CqnAnalyzerUtil cqnAnalyzerUtil;
    @Autowired
    @Qualifier("MasterDataService")
    private CdsService cdsService;

    public static final Logger logger = LoggerFactory.getLogger(DefectGroupHandler.class);

    /**
     * This method is used to perform Business Validation
     * 
     * @param context      cdsCreateContext
     * @param defectGroups provided defectGroups
     */
    @Before(event = CdsService.EVENT_CREATE, entity = DefectGroups_.CDS_NAME)
    public void beforeDefectGroupCreate(CdsCreateEventContext context, DefectGroups defectGroups) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeDefectGroupCreate");
        defectGroupValidator.checkInputsSanitized(defectGroups);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeDefectGroupCreate");
    }

    /**
     * This method is used to Create DefectGroup records
     * If record exists, updates the existing defect group
     * 
     * @param context      cdsCreateContext
     * @param defectGroups provided defectGroups
     */
    @On(event = CdsService.EVENT_CREATE, entity = DefectGroups_.CDS_NAME)
    public void onDefectGroupCreate(CdsCreateEventContext context, DefectGroups defectGroups) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onDefectGroupCreate");
        DefectGroups defectGroup = defectGroupService.fetchDefectGroupCode(defectGroups.getCode());
        if (null != defectGroup) {
            CqnUpdate update = Update.entity(DefectGroups_.class).data(defectGroups);
            context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onDefectGroupCreate");
    }

    /**
     * This method is used to update DefectGroup records
     * 
     * @param context
     * @param defectGroups
     */
    @Before(event = CdsService.EVENT_UPDATE, entity = DefectGroups_.CDS_NAME)
    public void updateDefectGroup(CdsUpdateEventContext context, DefectGroups defectGroups) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "updateDefectGroup");
        defectGroupValidator.checkInputsSanitized(defectGroups);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "updateDefectGroup");
    }

    @Before(event = CdsService.EVENT_DELETE, entity = DefectGroups_.CDS_NAME)
    public void beforeDefectGroupDelete(CdsDeleteEventContext context) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteDefectGroup");
        Map<String, Object> keys = cqnAnalyzerUtil.provideTargetKeys(context);
        String code = (String) keys.get(DefectGroups.CODE);
        defectGroupValidator.checkIfDefectCodeExistForDefectGroup(code);
        messages.throwIfError();
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteDefectGroup");
    }

}