package com.sap.ic.cmh.costcollector.handler;

import cds.gen.costcollectorservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.costcollector.validations.CostCollectorValidation;
import com.sap.ic.cmh.network.service.HttpService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.HashMap;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;

public class AddCostSUBLHandlerTest {

	@InjectMocks
	@Autowired
	AddCostSUBLHandler addcostsubhandler;

    @Mock 
    private AuditLogDifference auditLogDifference;

    @Mock
    AuditLogHelper auditLogHelper;
    
	@Mock
	AddCostContextSUBL addcostsubcontext;
	@Mock
	Messages messages;

	@Mock
	ComplaintService complaintService;
	@Mock
	AddCostFRHandler frHandler;
	@Mock
	CdsService cdsServ;
	@Mock
	Result result;
	@Mock
	CqnSelect sel;
	@Mock
	Complaints comp;
	@Mock
	Object object;
	@Mock
	HttpService httpService;
	@Mock
	CostCollectorService costCollectorService;
	@Mock
	CostCollectorValidation costCollectorValidation;

	private Object Unit = 10;
	private Object Currency = "EUR";
	private Object description = "faulty";
	private Object totalCost = 100;
	private Object itemType = "Panel";
	private Object subItemType = "plastic";
	private Object quantity = 15;
    private CostCollectors costCollector;
	HashMap<String, Object> map = new HashMap<>();
	String EXCHANGE_RATE_APPENDED_URL = "/http/getExchangeRate";

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		comp = Struct.create(Complaints.class);
		long l = 234;
		comp.setCreatedAt(Instant.ofEpochMilli(l));
		comp.setCurrencyCode("EUR");

		costCollector = Struct.create(CostCollectors.class);
		costCollector.setCurrencyCode("EUR");
        costCollector.setId("test-id");
	}

	@Test
	public void testConfirm() throws IOException {
		ResponseModel responseModel = new ResponseModel();
		responseModel.setResult("9999");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("");
		String exchangeRate = "100";
		Mockito.when(addcostsubcontext.getCqn()).thenReturn(sel);
		Mockito.when(addcostsubcontext.getService()).thenReturn(cdsServ);
		Mockito.when(cdsServ.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(Complaints.class)).thenReturn(comp);
		Mockito.when(addcostsubcontext.get(CostCollectors.CURRENCY_CODE)).thenReturn(Currency.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
		Mockito.when(costCollectorService.createExchangeRateDTO("M", costCollector.getCurrencyCode(),
				comp.getCurrencyCode())).thenReturn(map);
                Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
                Mockito.when(httpService.callCpiFlow(EXCHANGE_RATE_APPENDED_URL, map, "SAP_ASD")).thenReturn(responseModel);
		addcostsubhandler.confirm(addcostsubcontext);
		assertEquals(comp.getId(), costCollector.getParentId());
	}

	@Test(expected = Exception.class)
	public void testConfirmCurrencyAndTotalCostNull() throws IOException {
		ResponseModel responseModel = new ResponseModel();
		responseModel.setResult("9999");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("");
		String exchangeRate = "100";
		Mockito.when(addcostsubcontext.getCqn()).thenReturn(sel);
		Mockito.when(addcostsubcontext.getService()).thenReturn(cdsServ);
		Mockito.when(cdsServ.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(Complaints.class)).thenReturn(comp);
		Mockito.when(addcostsubcontext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
		Mockito.when(costCollectorService.createExchangeRateDTO("M", costCollector.getCurrencyCode(),
				comp.getCurrencyCode())).thenReturn(map);
		Mockito.when(httpService.callCpiFlow(EXCHANGE_RATE_APPENDED_URL, map, "SAP_ASD")).thenReturn(responseModel);
		addcostsubhandler.confirm(addcostsubcontext);
		assertEquals(comp.getId(), costCollector.getParentId());
	}

	@Test
	public void testConfirmExchangeRateNull() throws IOException {
		ResponseModel responseModel = new ResponseModel();
		responseModel.setResult("9999");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("");
		String exchangeRate = "100";
		Mockito.when(addcostsubcontext.getCqn()).thenReturn(sel);
		Mockito.when(addcostsubcontext.getService()).thenReturn(cdsServ);
		Mockito.when(cdsServ.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(Complaints.class)).thenReturn(comp);
		Mockito.when(addcostsubcontext.get(CostCollectors.CURRENCY_CODE)).thenReturn(Currency.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
		Mockito.when(costCollectorService.createExchangeRateDTO("M", costCollector.getCurrencyCode(),
				comp.getCurrencyCode())).thenReturn(map);
		Mockito.when(httpService.callCpiFlow(EXCHANGE_RATE_APPENDED_URL, map, "SAP_ASD")).thenReturn(null);
		Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        addcostsubhandler.confirm(addcostsubcontext);
		assertEquals(comp.getId(), costCollector.getParentId());
	}

	@Test
	public void testConfirmCurrencyCodeNotEqual() throws IOException {
		ResponseModel responseModel = new ResponseModel();
		responseModel.setResult("9999");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("");
		String exchangeRate = "100";
		comp.setCurrencyCode("INR");
		Mockito.when(addcostsubcontext.getCqn()).thenReturn(sel);
		Mockito.when(addcostsubcontext.getService()).thenReturn(cdsServ);
		Mockito.when(cdsServ.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(Complaints.class)).thenReturn(comp);
		Mockito.when(addcostsubcontext.get(CostCollectors.CURRENCY_CODE)).thenReturn(Currency.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
		Mockito.when(costCollectorService.createExchangeRateDTO("M", costCollector.getCurrencyCode(),
				comp.getCurrencyCode())).thenReturn(map);
		Mockito.when(httpService.callCpiFlow(EXCHANGE_RATE_APPENDED_URL, map, "SAP_ASD")).thenReturn(responseModel);
		Mockito.when(costCollectorService.getCostCollector(costCollector.getId())).thenReturn(costCollector);
        addcostsubhandler.confirm(addcostsubcontext);
		assertEquals(comp.getId(), costCollector.getParentId());
	}

	@Test
	public void testConfirmException() throws IOException {

		Mockito.when(addcostsubcontext.getCqn()).thenReturn(sel);
		Mockito.when(addcostsubcontext.getService()).thenReturn(cdsServ);
		Mockito.when(cdsServ.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.single(Complaints.class)).thenReturn(comp);
		Mockito.when(addcostsubcontext.get(CostCollectors.TOTAL_COST)).thenReturn(totalCost.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.ITEM_TYPE_CODE)).thenReturn(itemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.SUB_ITEM_TYPE_CODE)).thenReturn(subItemType.toString());
		Mockito.when(addcostsubcontext.get(CostCollectors.CURRENCY_CODE)).thenReturn(subItemType.toString());
		Mockito.when(costCollectorService.createExchangeRateDTO("M", costCollector.getCurrencyCode(),
				comp.getCurrencyCode())).thenReturn(map);
		Mockito.when(httpService.callCpiFlow(any(), any(), any())).thenThrow(IOException.class);

		addcostsubhandler.confirm(addcostsubcontext);
		assertEquals(comp.getId(), costCollector.getParentId());
	}

	@Test
	public void testValidateCostCollectorFields() {
		addcostsubhandler.validateSUBLUserAttributes(totalCost, description, Currency, costCollector);
		assertEquals(description.toString(), getCostCollectorObj().getDescription());
		assertEquals(totalCost.toString(), getCostCollectorObj().getTotalCost().toString());
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
		return tempCostObj;
	}

}
