package com.sap.ic.cmh.costcollector.service;

import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.model.ResponseModel;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterialUnits;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.costcollector.model.ExchangeRateRequest;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.utils.CommonFunctions;
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
import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;



public class CostCollectorServiceTest {


   @InjectMocks  @Autowired
    CostCollectorServiceImpl costCollectorService;
    @Mock
    CostCollectorDao costColDao;

    @Mock
    Result result;
    @Mock
    private Message msg;
    @Mock
    CommonFunctions commonFunctions;

    @Mock
    DestinationConfigurationDao destinationConfigDao;

    @Mock
    HttpService httpService;

    @Mock
    Messages messages;

    @Mock
    ServiceMaterials serviceMaterial;

    @Mock
    ServiceMaterialDao serviceMaterialDao;

    @Mock
    ServiceMaterialUnitDao serviceMaterialUnitDao;

    private CostCollectors costCollector;
    private cds.gen.complaintservice.Complaints comp;
    private List<CostCollectors> listofCostCol = new ArrayList<CostCollectors>();
    private Row row;
    private Optional<Row> opt;
    private ExchangeRateRequest exchRateRequest;
    private Map<String,Object> map = new HashMap<>();
    
    @Before
    public void beforeClass() {
    	
    	MockitoAnnotations.openMocks(this);
        comp = Struct.create(cds.gen.complaintservice.Complaints.class);
        long l = 234;
        comp.setCreatedAt(Instant.ofEpochMilli(l));
        comp.setCurrencyCode("EUR");
    	
    	costCollector = Struct.create(CostCollectors.class);
    	costCollector.setClaim("CID123");
    	costCollector.setDescription("faulty prod");
    	costCollector.setId("C101");
        costCollector.setCurrencyCode("EUR");
        costCollector.setTotalCost(BigDecimal.TEN);
        costCollector.setQuantity(BigDecimal.TEN);
        costCollector.setUnitCode("TAG");
    	
    	listofCostCol.add(costCollector);

        row = Struct.create(Row.class);

        exchRateRequest = new ExchangeRateRequest();
        exchRateRequest.setRateType("M");
        exchRateRequest.setFromCurrency("EUR");
        exchRateRequest.setToCurrency("INR");

        exchRateRequest.getRateType();
        exchRateRequest.getFromCurrency();
        exchRateRequest.getToCurrency();
        map.put("ID" ,new Object());
    }

    @Test
    public void testInsertCostCollector() {
    	when(costColDao.insertCostCollector(costCollector)).thenReturn(costCollector);
    	assertEquals(costCollector, costCollectorService.insertCostCollector(costCollector));        
    }

    @Test
    public void testUpdateCostCollector() {
    	when(costColDao.updateCostCollector(costCollector)).thenReturn(costCollector);
    	assertEquals(costCollector, costCollectorService.updateCostCollector(costCollector));
    }

    @Test
    public void testDeleteCostCollector() {
    	when(costColDao.deleteCostCollector(costCollector)).thenReturn(costCollector);
        assertEquals(costCollector, costCollectorService.deleteCostCollector(costCollector));
    }

    @Test
    public void testSelectTransferToClaimCostCollector()
    {
        when(costColDao.selectTransferToClaimCostCollector("C1234")).thenReturn(result);
        costCollectorService.selectTransferToClaimCostCollector("C1234");
    }
    @Test
    public void testSelectTransferToClaimCostCollectorForResultNotNull()
    {
        when(costColDao.selectTransferToClaimCostCollector("C1234")).thenReturn(result);
        opt = Optional.of(row);
        when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
        when(result.first()).thenReturn(opt);
        costCollectorService.selectTransferToClaimCostCollector("C1234");
    }

    @Test
    public void testSelectAllCostCollector()
    {
        when(costColDao.selectAllCostCollector("C1234")).thenReturn(result);
        costCollectorService.selectAllCostCollector("C1234");
    }

    @Test
    public void testSelectAllCostCollectorResultHasValues()
    {
        when(costColDao.selectAllCostCollector("C1234")).thenReturn(result);
        row.put("isRelevant", "ComplaintID");
        opt = Optional.of(row);
        when(result.listOf(CostCollectors.class)).thenReturn(listofCostCol);
        when(result.first()).thenReturn(opt);
        costCollectorService.selectAllCostCollector("C1234");
    }

    @Test
    public void testCalculateExchangeRateAndConvertCostDestinationNull() throws IOException {
        ResponseModel responseModel=new ResponseModel();
         responseModel.setResult("200");
         responseModel.setErrorMessage("");
         responseModel.setStatusCode("");
        costCollector.setCurrencyCode("EUR");
        comp.setCurrencyCode("INR");
        map.put("destination", comp);
        Mockito.when(commonFunctions.convertObjectToMap(exchRateRequest)).thenReturn(map);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType("","")).thenReturn(null);
        Mockito.when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
    }

    @Test
    public void testCalculateExchangeRateAndConvertCostBlank() throws IOException {
        costCollector.setCurrencyCode("EUR");
        comp.setCurrencyCode("INR");
        map.put("destination", comp);
        Mockito.when(commonFunctions.convertObjectToMap(exchRateRequest)).thenReturn(map);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType("","")).thenReturn(result);
        Mockito.when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(null);
        costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
    }

    @Test
    public void testCalculateExchangeRateAndConvertCost() throws IOException {
         ResponseModel responseModel=new ResponseModel();
         responseModel.setResult("200");
         responseModel.setErrorMessage("");
         responseModel.setStatusCode("");
        costCollector.setCurrencyCode("EUR");
        comp.setCurrencyCode("INR");
        map.put("destination", comp);
        Mockito.when(commonFunctions.convertObjectToMap(exchRateRequest)).thenReturn(map);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType("","")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        Mockito.when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
    }

     @Test
     public void testCalculateExchangeRateAndConvertCostCurrencyCode() {
         costCollector.setCurrencyCode("EUR");
         costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
     }

    @Test
    public void testCalculateExchangeRateAndConvertCostException() throws IOException {
        costCollector.setCurrencyCode("EUR");
        comp.setCurrencyCode("INR");
        Mockito.when(commonFunctions.convertObjectToMap(exchRateRequest)).thenReturn(map);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType("","")).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        Mockito.when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenThrow(IOException.class);
        costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
    }

    @Test
    public void testCalculateExchangeRateAndConvertCostDestinationNotNull() throws IOException {
        ResponseModel responseModel=new ResponseModel();
        responseModel.setResult("200");
        responseModel.setErrorMessage("Error");
        responseModel.setStatusCode("F456");
        costCollector.setCurrencyCode("EUR");
        comp.setCurrencyCode("INR");
        Mockito.when(commonFunctions.convertObjectToMap(exchRateRequest)).thenReturn(map);
        Mockito.when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(),any())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        Mockito.when(httpService.callCpiFlow(any(String.class), any(Map.class), any(String.class))).thenReturn(responseModel);
        costCollectorService.calculateExchangeRateAndConvertCost(costCollector, comp);
    }

    @Test
    public void testGetCostCollector() {
        when(costColDao.getCostCollector(any())).thenReturn(result);
        when(result.single(CostCollectors.class)).thenReturn(costCollector);
        costCollectorService.getCostCollector("1234");
    }

    @Test
    public void fetchConfiguredDestinationTest() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(),any())).thenReturn(result);
        costCollectorService.fetchConfiguredDestination("1234","89");
    }

    @Test
    public void fetchConfiguredDestinationNullTest() {
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(),any())).thenReturn(null);
        costCollectorService.fetchConfiguredDestination("1234","89");
    }

    @Test
    public void convertQuantityOfFRNullTest() {
        when(serviceMaterialDao.getServiceMaterialsBasedOnSubItemType(any())).thenReturn(null);
        costCollectorService.convertQuantityOfFR(costCollector);
    }

    @Test
    public void convertQuantityOfFRTest() {
        ServiceMaterialUnits serviceMaterialUnit = Struct.create(ServiceMaterialUnits.class);
        serviceMaterialUnit.setNumerator(new BigDecimal(11));
        serviceMaterialUnit.setDenominator(new BigDecimal(15));
        ServiceMaterials serviceMaterial = Struct.create(ServiceMaterials.class);
        serviceMaterial.setServiceMaterial("WTY-FR");
        List<ServiceMaterials> sMList = new ArrayList<>();
        sMList.add(serviceMaterial);
        List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("serviceMaterial", "WTR-TR");
		rowValues.add(row);
		when(result.first()).thenReturn(Optional.of(row));
        when(serviceMaterialDao.getServiceMaterialsBasedOnSubItemType(any())).thenReturn(result);
        when(serviceMaterialUnitDao.getServiceMaterialUnitBasedOnMaterialIdAndUnit(any(), any())).thenReturn(result);
        when(result.single(ServiceMaterialUnits.class)).thenReturn(serviceMaterialUnit);
        when(result.listOf(ServiceMaterials.class)).thenReturn(sMList);
        costCollectorService.convertQuantityOfFR(costCollector);
    }
    @Test
    public void testConvertQuantityOfFRNull() {
        when(messages.error(MessageKeys.UNIT_NOT_CONFIGURED_IN_SERVICE_MATERIAL_UNIT)).thenReturn(msg);
        costCollectorService.convertQuantityOfFR(costCollector);
    }
}