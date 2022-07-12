package com.sap.ic.cmh.costcollector.handler;

import java.util.List;
import java.util.Map;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import org.slf4j.Logger;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.web.context.annotation.RequestScope;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.costcollectorservice.CostCollectors_;
import com.sap.ic.cmh.gen.MessageKeys;

import java.math.BigDecimal;

@RequestScope
@Component
@ServiceName("CostCollectorService")
public class EditCostFRHandler implements EventHandler {

    @Autowired
    Messages messages;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    ComplaintService complaintService;

    @Autowired
    AddCostFRHandler handlerFR;

    @Autowired
    AddCostSUBLHandler handlerSUBL;

    @Autowired
    ClaimService claimService;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<CostCollectors> auditLogDifference;

    private static final String ENTITY_NAME = "COSTCOLLECTOR";
    public static final Logger logger = LoggerHelper.getLogger(EditCostFRHandler.class);
   	private static final String EDIT_COST_FR_HANDLER = "EditCostFRHandler";
   	private static final String CONFIRM = "confirm";

    /**
     * Handler to Update the Cost Collector entity
     * 
     * @param context
     */
    @On(entity = CostCollectors_.CDS_NAME)
    public void confirm(EditCostFRContext context) {
      LoggerHelper.logMethodEntry(logger, EDIT_COST_FR_HANDLER, CONFIRM);
        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        CostCollectors costCollector =
                (service).run(select).single(CostCollectors.class);
                setOldAuditData(costCollector);
        try {
            Object description = context.get(CostCollectors.DESCRIPTION);
            Object unit = context.get(CostCollectors.UNIT_CODE);
            Object quantity = context.get(CostCollectors.QUANTITY);
            BigDecimal oldQuantity = costCollector.getQuantity().negate();
            if(!costCollector.getUnitCode().equals(Constants.DEFAULT_LABOR_UNIT)){
                oldQuantity = costCollectorService.convertQuantityOfFR(costCollector).negate();
            }
            complaintService.updateComplaintWithQuantity(costCollector.getParentId(), oldQuantity);
            handlerFR.validateFRUserAttributes(quantity, unit, description, costCollector);
            BigDecimal newQuantity = costCollector.getQuantity();
            if(!costCollector.getUnitCode().equals(Constants.DEFAULT_LABOR_UNIT)){
                newQuantity = costCollectorService.convertQuantityOfFR(costCollector);
            }
            complaintService.updateComplaintWithQuantity(costCollector.getParentId(), newQuantity);
            updateCostCollector(costCollector);
            messages.success(MessageKeys.EDIT_SUCCESS);
            context.setResult(costCollector);
            context.setCompleted();
            logUpsert(costCollector);
        } catch (ODataException e) {
            messages.success(MessageKeys.UNSUCESSFUL_EDITION);
            context.setResult(costCollector);
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, EDIT_COST_FR_HANDLER, CONFIRM);
    }


    /**
     * update cost collector
     * 
     * @param costCollector
     */
    public void updateCostCollector(CostCollectors costCollector) {
    	LoggerHelper.logMethodEntry(logger, EDIT_COST_FR_HANDLER, CONFIRM);
    	costCollectorService.updateCostCollector(costCollector);
    	LoggerHelper.logMethodExit(logger, EDIT_COST_FR_HANDLER, CONFIRM);
    }

    public void setOldAuditData(CostCollectors costCollector) {
		CostCollectors oldData = costCollectorService.getCostCollector(costCollector.getId());
		auditLogDifference.setOldData(oldData);
	}
    
    public void logUpsert(CostCollectors costCollector) {
        CostCollectors newData = costCollectorService.getCostCollector(costCollector.getId());
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

}