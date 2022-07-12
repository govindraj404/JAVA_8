package com.sap.ic.cmh.costcollector.handler;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Map;
import java.util.List;
import com.sap.ic.cmh.auditlog.ObjectDiff;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.costcollector.validations.CostCollectorValidation;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.utils.Constants;

import cds.gen.costcollectorservice.Complaints;
import cds.gen.costcollectorservice.Complaints_;
import cds.gen.costcollectorservice.CostCollectors;

@RequestScope
@Component
@ServiceName("CostCollectorService")
public class AddCostSUBLHandler implements EventHandler {

    @Autowired
    Messages messages;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
    ComplaintService complaintService;

    @Autowired
    AddCostFRHandler frHandler;

    @Autowired
    HttpService httpService;

    @Autowired
    CostCollectorValidation costCollectorValidation;

    @Autowired
	AuditLogHelper auditLogHelper;

    @Autowired
	private AuditLogDifference<CostCollectors> auditLogDifference;

    private static final String ENTITY_NAME = "COSTCOLLECTOR";

    /**
     * Handler to add the Cost Collector entity
     *
     * @param context
     */
    @On(entity = Complaints_.CDS_NAME)
    public void confirm(AddCostContextSUBL context) {
        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        Complaints comp = (service).run(select).single(Complaints.class);
        try {
            CostCollectors costCollector = Struct.create(CostCollectors.class);
            Object description = context.get(CostCollectors.DESCRIPTION);
            Object totalCost = context.get(CostCollectors.TOTAL_COST);
            Object currency = context.get(CostCollectors.CURRENCY_CODE);
            Object itemType = context.get(CostCollectors.ITEM_TYPE_CODE);
            Object subItemType = context.get(CostCollectors.SUB_ITEM_TYPE_CODE);
            costCollector.setSubItemTypeCode(subItemType.toString());
            validateSUBLUserAttributes(totalCost, currency, description, costCollector);
            String exchangeRate = "1";
            ResponseModel responseFromCpi = null;
            if (!costCollector.getCurrencyCode().equals(comp.getCurrencyCode())) {
                Map<String, Object> exchRateRequestMap = costCollectorService.createExchangeRateDTO("M",
                        costCollector.getCurrencyCode(), comp.getCurrencyCode());
                responseFromCpi = httpService.callCpiFlow(Constants.EXCHANGE_RATE_APPENDED_URL, exchRateRequestMap,
                        costCollectorService.fetchConfiguredDestination(comp.getCompanyCodeId(), Constants.CLAIM_CODE));
                exchangeRate = null != responseFromCpi ? responseFromCpi.getResult() : null;
            }
            if (null != exchangeRate) {
                BigDecimal rate = new BigDecimal(exchangeRate);
                BigDecimal tcost = new BigDecimal(totalCost.toString());
                BigDecimal tc = tcost.multiply(rate);
                frHandler.insertCostCollector(costCollector, comp.getId(), subItemType.toString(), itemType.toString());
                complaintService.updateComplaintWithCost(costCollector.getParentId(), tc);
                messages.success(MessageKeys.INSERTION_SUCCESS);
                context.setResult(comp);
                context.setCompleted();
            } else {
                messages.error(MessageKeys.UNSUCESSFUL_INSERTION);
            }
            messages.throwIfError();
            logUpsert(costCollector);
        } catch (IOException e) {
            messages.success(MessageKeys.UNSUCESSFUL_INSERTION);
            context.setResult(comp);
            context.setCompleted();
        }
    }

    /**
     * Validate Input fields
     *
     * @param description
     * @param totalCost
     * @param itemType
     * @param subItemType
     * @param quantity
     * @param costCollector
     */
    public void validateSUBLUserAttributes(Object totalCost, Object currency, Object description,
            CostCollectors costCollector) {
        if (description != null) {
            costCollector.setDescription(description.toString());
        }
        if (totalCost != null) {
            costCollector.setTotalCost(new BigDecimal(totalCost.toString()));
        }
        if (currency != null) {
            costCollector.setCurrencyCode(currency.toString());
        }
        costCollectorValidation.validateCostCollectorSUBLAttributes(costCollector);
        messages.throwIfError();
    }

	public void logUpsert(CostCollectors costCollector) {
		List<ObjectDiff> diffList = auditLogDifference.getDifference(costCollector);
		Map<String, String> entityInfoMap = auditLogHelper.buildEntityInfoMap(
				ENTITY_NAME,
				costCollector.getId());
		auditLogHelper.logUpsertAuditData(diffList, entityInfoMap);
	}
}