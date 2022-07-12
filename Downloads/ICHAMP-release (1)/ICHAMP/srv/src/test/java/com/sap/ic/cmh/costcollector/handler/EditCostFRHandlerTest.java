package com.sap.ic.cmh.costcollector.handler;

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
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;

public class EditCostFRHandlerTest {
	
	
	@InjectMocks @Autowired
	EditCostFRHandler editCostFRhand;
	
	@Mock
	EditCostFRContext editCostFRcontext;
	@Mock
	Messages messages;
	@Mock
	CostCollectorService costCollectorService;
	@Mock
	ComplaintService complaintService;
	@Mock
	AddCostFRHandler handlerFR;
	@Mock
	AddCostSUBLHandler handlerSUBL;
	@Mock
	CqnSelect sel;
	@Mock
	CdsService service;
	@Mock
	Result result;
	@Mock
	CostCollectors costCollector;

	@Mock
	AuditLogHelper auditLogHelper;

    @Mock 
    private AuditLogDifference auditLogDifference;
	
	private Object Unit = 10;	
	private Object description = "faulty";
	private Object quantity = 15;
	
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		costCollector = Struct.create(CostCollectors.class);
        costCollector.setId("test-id");
		costCollector.setQuantity(BigDecimal.TEN);
		costCollector.setUnitCode("TAG");
	}

	
	@Test
	public void testConfirm() {
		
		Mockito.when(editCostFRcontext.getCqn()).thenReturn(sel);
		Mockito.when(editCostFRcontext.getService()).thenReturn(service);
		Mockito.when(service.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(editCostFRcontext.get(CostCollectors.UNIT_CODE)).thenReturn(Unit.toString());
		Mockito.when(editCostFRcontext.get(CostCollectors.QUANTITY)).thenReturn(quantity.toString());
		Mockito.when(editCostFRcontext.get(CostCollectors.DESCRIPTION)).thenReturn(description.toString());
		Mockito.when(costCollectorService.convertQuantityOfFR(costCollector)).thenReturn(BigDecimal.TEN.negate());
		Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        editCostFRhand.confirm(editCostFRcontext);	
		assertEquals(getcostObj().getUnitCode(), Unit.toString());
	}

	@Test
	public void testConfirmFail() {

		Mockito.when(editCostFRcontext.getCqn()).thenReturn(sel);
		Mockito.when(editCostFRcontext.getService()).thenReturn(service);
		Mockito.when(service.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(editCostFRcontext.get(CostCollectors.UNIT_CODE)).thenReturn(Unit.toString());
		Mockito.when(editCostFRcontext.get(CostCollectors.QUANTITY)).thenReturn(quantity.toString());
		Mockito.when(editCostFRcontext.get(CostCollectors.DESCRIPTION)).thenReturn(description.toString());
		Mockito.when(costCollectorService.updateCostCollector(any())).thenThrow(ODataException.class);
		Mockito.when(costCollectorService.convertQuantityOfFR(costCollector)).thenReturn(BigDecimal.TEN.negate());
		editCostFRhand.confirm(editCostFRcontext);
	}

	private CostCollectors getcostObj() {
		
		CostCollectors temp = Struct.create(CostCollectors.class);
		temp.setUnitCode(Unit.toString());
		temp.setQuantity(new BigDecimal(15));
		temp.setDescription(description.toString());
		return temp;
		
	}

}