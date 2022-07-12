package com.sap.ic.cmh.costcollector.handler;

import cds.gen.complaintservice.Complaints;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.utils.Constants;
import org.slf4j.Logger;
import java.util.List;
import java.util.Map;
import com.sap.cds.Struct;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.costcollectorservice.CostCollectors_;
import com.sap.ic.cmh.gen.MessageKeys;

import java.math.BigDecimal;

@RequestScope
@Component
@ServiceName("CostCollectorService")
public class DeleteCostHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    ComplaintService complaintService;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<CostCollectors> auditLogDifference;

    private static final String ENTITY_NAME = "COSTCOLLECTOR";
    public static final Logger logger = LoggerHelper.getLogger(DeleteCostHandler.class);
    private static final String DELETE_COST_HANDLER = "DeleteCostHandler";

    /**
     * Handler to delete the Cost Collector entity
     * 
     * @param context
     */
    @On(entity = CostCollectors_.CDS_NAME)
    public void confirm(DeleteCostContext context) {
        LoggerHelper.logMethodEntry(logger, DELETE_COST_HANDLER, "confirm");
        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        CostCollectors cost = (service).run(select).single(CostCollectors.class);
        setOldAuditData(cost);
        try {
            Complaints comp = complaintService.getComplaintDetails(cost.getParentId());
            if (cost.getTotalCost() != null) {
                BigDecimal convertedCost = costCollectorService.calculateExchangeRateAndConvertCost(cost, comp);
                if (convertedCost != null) {
                    BigDecimal totalCost = convertedCost.negate();
                    costCollectorService.deleteCostCollector(cost);
                    complaintService.updateComplaintWithCost(cost.getParentId(), totalCost);
                    messages.success(MessageKeys.DELETION_SUCCESS);
                    context.setResult(cost);
                    context.setCompleted();
                }
            }
           if(cost.getQuantity()!=null){
            costCollectorService.deleteCostCollector(cost);
            BigDecimal totalQuantity = new BigDecimal(cost.getQuantity().toString()).negate();
            if(!cost.getUnitCode().equals(Constants.DEFAULT_LABOR_UNIT)){
                totalQuantity = costCollectorService.convertQuantityOfFR(cost).negate();
            }
            complaintService.updateComplaintWithQuantity(cost.getParentId(),totalQuantity);
            messages.success(MessageKeys.DELETION_SUCCESS);
            context.setResult(cost);
            context.setCompleted();
           }
           logUpsert();
        } catch (ODataException e) {
            messages.success(MessageKeys.UNSUCESSFUL_DELETION);
            context.setResult(cost);
            context.setCompleted();
        }
        LoggerHelper.logMethodExit(logger, DELETE_COST_HANDLER, "confirm");
    }

    public void setOldAuditData(CostCollectors costCollector) {
		auditLogDifference.setOldData(costCollector);
	}

    public void logUpsert() {
        CostCollectors newData = Struct.create(CostCollectors.class);
		List<ObjectDiff> diffList = auditLogDifference.getDifference(newData);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				newData.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}

}
