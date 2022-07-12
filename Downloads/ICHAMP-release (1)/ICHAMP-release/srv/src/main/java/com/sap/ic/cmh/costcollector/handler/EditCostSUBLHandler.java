package com.sap.ic.cmh.costcollector.handler;

import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.costcollectorservice.CostCollectors_;
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
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import org.slf4j.Logger;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import java.math.BigDecimal;

@RequestScope
@Component
@ServiceName("CostCollectorService")
public class EditCostSUBLHandler implements EventHandler {

    @Autowired
    Messages messages;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    ComplaintService complaintService;

    @Autowired
    EditCostFRHandler handlerFR;

    @Autowired
    AddCostSUBLHandler addHandlerSUBL;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<CostCollectors> auditLogDifference;

    private static final String ENTITY_NAME = "COSTCOLLECTOR";
    public static final Logger logger = LoggerHelper.getLogger(EditCostSUBLHandler.class);
	private static final String EDIT_COST_SUBLET_HANDLER = "EditCostSUBLHandler";

    /**
     * Handler to Update the Cost Collector entity
     * 
     * @param context
     */
    @On(entity = CostCollectors_.CDS_NAME)
    public void confirm(EditCostSUBLContext context) {
		LoggerHelper.logMethodEntry(logger, EDIT_COST_SUBLET_HANDLER, "confirm");
		CqnSelect select = context.getCqn();
		CdsService service = context.getService();
		CostCollectors costCollector = (service).run(select).single(CostCollectors.class);
        setOldAuditData(costCollector);
		try {
			Complaints comp = complaintService.getComplaintDetails(costCollector.getParentId());

			Object description = context.get(CostCollectors.DESCRIPTION);
			Object totalCost = context.get(CostCollectors.TOTAL_COST);
			Object currency = context.get(CostCollectors.CURRENCY_CODE);
			BigDecimal convertedOldCost = costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
			BigDecimal oldTotalCost = convertedOldCost.negate();
			complaintService.updateComplaintWithCost(costCollector.getParentId(), oldTotalCost);
			logger.info("***********The oldtotalCost in Edit Cost is ::: {}", oldTotalCost);
			addHandlerSUBL.validateSUBLUserAttributes(totalCost, currency, description, costCollector);

			BigDecimal newTotalCost = costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
			logger.info("***********The newtotalCost in Edit Cost is ::: {}", newTotalCost);
			complaintService.updateComplaintWithCost(costCollector.getParentId(), newTotalCost);
			handlerFR.updateCostCollector(costCollector);
			messages.success(MessageKeys.EDIT_SUCCESS);
			context.setResult(costCollector);
			context.setCompleted();
            logUpsert(costCollector);
		} catch (ODataException e) {
			messages.success(MessageKeys.UNSUCESSFUL_EDITION);
			context.setResult(costCollector);
			context.setCompleted();
		}
		LoggerHelper.logMethodExit(logger, EDIT_COST_SUBLET_HANDLER, "confirm");
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
