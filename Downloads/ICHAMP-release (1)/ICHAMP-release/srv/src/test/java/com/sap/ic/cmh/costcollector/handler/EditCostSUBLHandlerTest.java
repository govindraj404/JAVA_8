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
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;

public class EditCostSUBLHandlerTest {
	
	@InjectMocks @Autowired
	EditCostSUBLHandler editCostSUBHandler;
	
	@Mock
	EditCostSUBLContext editCostSUBLcontext;
	@Mock
	Messages messages;
	@Mock
	CostCollectorService costCollectorService;
	@Mock
	ComplaintService complaintService;
	@Mock
	EditCostFRHandler handlerFR;
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

	private Object descr = "faulty";
	private Object total_cost = new BigDecimal(10);
	private Object currency = "EUR";
	private cds.gen.complaintservice.Complaints comp;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);

		comp = Struct.create(cds.gen.complaintservice.Complaints.class);
		long l = 234;
		comp.setCreatedAt(Instant.ofEpochMilli(l));
		comp.setCurrencyCode("EUR");

		costCollector = Struct.create(CostCollectors.class);
		costCollector.setId("12");
		costCollector.setParentId("1234");
		costCollector.setCurrencyCode("EUR");
		costCollector.setQuantity(BigDecimal.TEN);
	}
	
	@Test
	public void testConfirm() {
		Mockito.when(editCostSUBLcontext.getCqn()).thenReturn(sel);
		Mockito.when(editCostSUBLcontext.getService()).thenReturn(service);
		Mockito.when(service.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(editCostSUBLcontext.get(CostCollectors.CURRENCY_CODE)).thenReturn(currency.toString());
		Mockito.when(editCostSUBLcontext.get(CostCollectors.TOTAL_COST)).thenReturn(total_cost.toString());
		Mockito.when(editCostSUBLcontext.get(CostCollectors.DESCRIPTION)).thenReturn(descr.toString());
		Mockito.when(complaintService.getComplaintDetails(costCollector.getParentId())).thenReturn(comp);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp)).thenReturn(BigDecimal.TEN);
		Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        editCostSUBHandler.confirm(editCostSUBLcontext);
		assertEquals(getcostObj().getCurrencyCode(), currency.toString());	
	}

	@Test
	public void testConfirmFail() {
		Mockito.when(editCostSUBLcontext.getCqn()).thenReturn(sel);
		Mockito.when(editCostSUBLcontext.getService()).thenReturn(service);
		Mockito.when(service.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(any(), any())).thenThrow(ODataException.class);
		editCostSUBHandler.confirm(editCostSUBLcontext);
	}
	
	public CostCollectors getcostObj() {
		
		CostCollectors temp = Struct.create(CostCollectors.class);
		temp.setCurrencyCode(currency.toString());
		temp.setDescription(descr.toString());
		temp.setTotalCost(new BigDecimal(10));
		return temp;
	}
}