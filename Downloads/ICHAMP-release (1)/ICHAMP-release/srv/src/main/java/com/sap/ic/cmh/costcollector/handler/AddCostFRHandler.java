package com.sap.ic.cmh.costcollector.handler;

import java.math.BigDecimal;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import com.sap.cds.Struct;
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
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.costcollector.validations.CostCollectorValidation;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.gen.MessageKeys;
import cds.gen.costcollectorservice.Complaints;
import cds.gen.costcollectorservice.Complaints_;
import cds.gen.costcollectorservice.CostCollectors;

@RequestScope
@Component
@ServiceName("CostCollectorService")
public class AddCostFRHandler implements EventHandler {

    @Autowired
    Messages messages;

    @Autowired
    CostCollectorService costCollectorService;

    @Autowired
	AuditLogHelper auditLogHelper;
    
    @Autowired
    ComplaintService complaintService;

    @Autowired
    CostCollectorValidation costCollectorValidation;

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    ServiceMaterialUnitDao serviceMaterialUnitDao;

    @Autowired
	private AuditLogDifference<CostCollectors> auditLogDifference;

    private static final String ENTITY_NAME = "COSTCOLLECTOR";


    /**
     * Handler to add the Cost Collector entity
     * 
     * @param context
     */
    @On(entity = Complaints_.CDS_NAME)
    public void confirm(AddCostContextFR context) {

        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        Complaints comp = (service).run(select).single(Complaints.class);
        try {
            CostCollectors costCollector = Struct.create(CostCollectors.class);
            Object description = context.get(CostCollectors.DESCRIPTION);
            Object unit = context.get(CostCollectors.UNIT_CODE);
            Object quantity = context.get(CostCollectors.QUANTITY);
            Object subItemType = context.get(CostCollectors.SUB_ITEM_TYPE_CODE);
            Object itemType = context.get(CostCollectors.ITEM_TYPE_CODE);
            costCollector.setSubItemTypeCode(subItemType.toString());
            validateFRUserAttributes(quantity, unit, description, costCollector);

            BigDecimal convertedQuantity = costCollectorService.convertQuantityOfFR(costCollector);
            if(convertedQuantity==null){
                messages.throwIfError();
            }
            else{
            insertCostCollector(costCollector, comp.getId(), subItemType.toString(), itemType.toString());
            complaintService.updateComplaintWithQuantity(costCollector.getParentId(), convertedQuantity);
            messages.success(MessageKeys.INSERTION_SUCCESS);
            context.setResult(comp);
            context.setCompleted();
            }
        logUpsert(costCollector);
        } catch (ODataException e) {
            messages.success(MessageKeys.UNSUCESSFUL_INSERTION);
            context.setResult(comp);
            context.setCompleted();
        }
    }

    /**
     * inserting the costcollector
     * 
     * @param costCollector
     * @param id
     * @param subItemType
     * @param itemType
     */
    public void insertCostCollector(CostCollectors costCollector, String id, String subItemType, String itemType) {
        costCollector.setParentId(id);
        costCollector.setSubItemTypeCode(subItemType);
        costCollector.setItemTypeCode(itemType);
        costCollectorService.insertCostCollector(costCollector);

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
    public void validateFRUserAttributes(Object quantity, Object unit, Object description,
            CostCollectors costCollector) {
        if (description != null) {
            costCollector.setDescription(description.toString());
        }
        if (quantity != null) {
            costCollector.setQuantity(new BigDecimal(quantity.toString()));
        }
        if (unit != null) {
            costCollector.setUnitCode(unit.toString());
        }
        costCollectorValidation.validateCostCollectorFRAttributes(costCollector);
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
