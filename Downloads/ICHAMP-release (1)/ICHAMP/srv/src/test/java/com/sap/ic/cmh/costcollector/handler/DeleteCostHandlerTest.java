package com.sap.ic.cmh.costcollector.handler;

import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
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
import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;



public class DeleteCostHandlerTest {
	
	@InjectMocks @Autowired
	DeleteCostHandler delcosthand;
	
	@Mock
	Messages messages;	
	@Mock
	CostCollectorService costCollectorService;
	@Mock
	ComplaintService complaintService;	
	@Mock
	DeleteCostContext delContext;	
	@Mock
	CqnSelect cqnSelect;	
	@Mock
	CdsService cdsService;
	@Mock
	Result result;
	@Mock
    Row row;
	@Mock
	PersistenceService db;
	@Mock
	CostCollectorDao costColDao;

	@Mock
	AuditLogHelper auditLogHelper;

    @Mock 
    private AuditLogDifference auditLogDifference;
    
	private Complaints comp;

	private CostCollectors costCollector;
	private List<CostCollectors> listofCostCol = new ArrayList<CostCollectors>(); 
	

	@Before
	public void beforeClass(){
		
		MockitoAnnotations.openMocks(this);
		comp = Struct.create(Complaints.class);
		long l = 234;
		comp.setCreatedAt(Instant.ofEpochMilli(l));
		comp.setCurrencyCode("EUR");
		
		costCollector = Struct.create(CostCollectors.class);
		costCollector.setTotalCost(BigDecimal.TEN);
		costCollector.setQuantity(BigDecimal.TEN);
		costCollector.setUnitCode("TAG");
		costCollector.setClaim("C123");
		costCollector.setCurrencyCode("EUR");
		costCollector.setParentId("P123");
		costCollector.setId("I123");
		
		listofCostCol.add(costCollector);
	}
	
	@Test
	public void testConfirm(){
		
		Mockito.when(delContext.getCqn()).thenReturn(cqnSelect);
		Mockito.when(delContext.getService()).thenReturn(cdsService);
		Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(complaintService.getComplaintDetails(costCollector.getParentId())).thenReturn(comp);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(costCollector,comp)).thenReturn(BigDecimal.TEN);
		Mockito.when(costCollectorService.convertQuantityOfFR(costCollector)).thenReturn(BigDecimal.TEN.negate());
		delcosthand.confirm(delContext);
	}

	@Test
	public void testConfirmConvertedCostNull(){

		Mockito.when(delContext.getCqn()).thenReturn(cqnSelect);
		Mockito.when(delContext.getService()).thenReturn(cdsService);
		Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(complaintService.getComplaintDetails(costCollector.getParentId())).thenReturn(comp);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(costCollector,comp)).thenReturn(null);
		Mockito.when(costCollectorService.convertQuantityOfFR(costCollector)).thenReturn(BigDecimal.TEN.negate());
		delcosthand.confirm(delContext);
	}

	@Test
	public void testConfirmForNullValues(){
		costCollector.setTotalCost(null);
		costCollector.setQuantity(null);
		Mockito.when(delContext.getCqn()).thenReturn(cqnSelect);
		Mockito.when(delContext.getService()).thenReturn(cdsService);
		Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(complaintService.getComplaintDetails(costCollector.getParentId())).thenReturn(comp);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(costCollector,comp)).thenReturn(BigDecimal.TEN);
		delcosthand.confirm(delContext);
	}

	@Test
	public void testOnFail() {
		Mockito.when(delContext.getCqn()).thenReturn(cqnSelect);
		Mockito.when(delContext.getService()).thenReturn(cdsService);
		Mockito.when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(CostCollectors.class)).thenReturn(costCollector);
		Mockito.when(complaintService.getComplaintDetails(costCollector.getParentId())).thenReturn(comp);
		Mockito.when(costCollectorService.calculateExchangeRateAndConvertCost(costCollector,comp)).thenReturn(BigDecimal.TEN);
		Mockito.when(costCollectorService.deleteCostCollector(any())).thenThrow(ODataException.class);
		delcosthand.confirm(delContext);
	}

}