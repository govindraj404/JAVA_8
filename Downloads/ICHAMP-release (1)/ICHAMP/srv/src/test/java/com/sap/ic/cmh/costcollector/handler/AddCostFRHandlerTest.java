package com.sap.ic.cmh.costcollector.handler;

import cds.gen.costcollectorservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.costcollector.validations.CostCollectorValidation;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;

public class AddCostFRHandlerTest {

    @InjectMocks
    @Autowired
    AddCostFRHandler addCostFRHandler;

    @Mock
    AddCostContextFR addcostContext;
    @Mock
    Messages messages;
    @Mock
    CostCollectorService costCollectorService;
    @Mock
    ComplaintService complaintService;

    @Mock 
    private AuditLogDifference auditLogDifference;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock
    Result result;
    @Mock
    CqnSelect select;
    @Mock
    CdsService cdsService;

    @Mock
    Complaints comp;
    @Mock
    CostCollectorValidation costCollectorValidation;

    private CostCollectors costCollector;
    private Object Unit = 10;
    private Object Currency = "EUR";
    private Object description = "faulty";
    private Object totalCost = 100;
    private Object itemType = "Panel";
    private Object subItemType = "plastic";
    private Object quantity = 15;
    private String ID = "I123";

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        costCollector = Struct.create(CostCollectors.class);
        costCollector.setId("test-id");
    }

    @Test
    public void testConfirm() {
        Mockito.when(addcostContext.getCqn()).thenReturn(select);
        Mockito.when(addcostContext.getService()).thenReturn(cdsService);
        Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.single(Complaints.class)).thenReturn(comp);
        Mockito.when(addcostContext.get(CostCollectors.QUANTITY)).thenReturn(quantity.toString());
        Mockito.when(addcostContext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
        Mockito.when(addcostContext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
        Mockito.when(addcostContext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
        Mockito.when(costCollectorService.convertQuantityOfFR(any())).thenReturn(new BigDecimal(10));
        Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        addCostFRHandler.confirm(addcostContext);
    }

    @Test
    public void testConfirmFail() {
        Mockito.when(addcostContext.getCqn()).thenReturn(select);
        Mockito.when(addcostContext.getService()).thenReturn(cdsService);
        Mockito.when(addcostContext.get(CostCollectors.QUANTITY)).thenReturn(quantity.toString());
        Mockito.when(addcostContext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
        Mockito.when(addcostContext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
        Mockito.when(addcostContext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
        Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.single(Complaints.class)).thenReturn(comp);
        Mockito.when(costCollectorService.insertCostCollector(any())).thenThrow(ODataException.class);
        Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        addCostFRHandler.confirm(addcostContext);

    }

    @Test
    public void testInsertCostCollector() {
        addCostFRHandler.insertCostCollector(getCostCollectorObj(), subItemType.toString(), ID, itemType.toString());
        assertEquals(getCostCollectorObj().getParentId(), ID);
        assertEquals(getCostCollectorObj().getSubItemTypeCode(), subItemType.toString());
        assertEquals(getCostCollectorObj().getItemTypeCode(), itemType.toString());
    }

    @Test
    public void testvalidateFRUserAttributess() {
        addCostFRHandler.validateFRUserAttributes(quantity, Unit, description, costCollector);
        assertEquals(description.toString(), getCostCollectorObj().getDescription());
        assertEquals(quantity.toString(), getCostCollectorObj().getQuantity().toString());
    }

    private CostCollectors getCostCollectorObj() {

        CostCollectors tempCostObj = Struct.create(CostCollectors.class);
        tempCostObj.setCurrencyCode(Currency.toString());
        tempCostObj.setUnitCode(Unit.toString());
        tempCostObj.setDescription(description.toString());
        tempCostObj.setItemTypeCode(itemType.toString());
        tempCostObj.setSubItemTypeCode(subItemType.toString());
        tempCostObj.setTotalCost(new BigDecimal(totalCost.toString()));
        tempCostObj.setQuantity(new BigDecimal(quantity.toString()));
        tempCostObj.setParentId(ID);
        return tempCostObj;
    }

    @Test
    public void testvalidateFRUserAttributessNull() {
        addCostFRHandler.validateFRUserAttributes(null, Unit, description, costCollector);

    }

}