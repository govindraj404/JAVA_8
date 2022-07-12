package com.sap.ic.cmh.configuration.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.managecomplaint.service.ManageComplaintService;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDetails;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.returnpo.model.ReturnOrderBackendStatus;
import com.sap.ic.cmh.returnpo.model.ReturnPurchaseOrderRequestPayload;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;

import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.masterdataservice.Plants;

public class MessageServiceTest {
    @InjectMocks
    MessageService handler;
    @Mock
    ManageComplaintService manageComplaintService;
    @Mock
    MasterDataService masterDataService;
    @Mock
    DestinationConfigurationDao destinationConfigDao;
    @Mock
    ConfigurationDao configurationDao;
    @Mock
    Messages messages;
    @Mock
    private Message msg;

    @Mock
    Result result;
    @Mock
    CdsService claimCdsService;
    @Mock
    CdsService qnCdsService;
    @Mock
    CdsService returnOrderCdsService;
    @Mock
	ClaimService internalClaimService;
    @Mock
	ReturnPurchaseOrderService returnPurchaseOrderService;
    @Mock
    QualityNotificationService qualityNotificationService;
    private Optional<Row> opt;

    private Row row;

    private QualityNotificationDetails qualityNotificationDetails;
    private Complaints  complaints;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        row = Struct.create(Row.class);
        complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("12");
        complaints.setMaterialId("201");
        complaints.setId("test");
        qualityNotificationDetails= new QualityNotificationDetails();
        qualityNotificationDetails.setMaterial("test");
        qualityNotificationDetails.setNotificationType("test");
        qualityNotificationDetails.setSupplier("sup");
        qualityNotificationDetails.setPlant("paltn");
        qualityNotificationDetails.setPurchaseOrderNumber("Pr");
        qualityNotificationDetails.setPersonResponsible("res");
    }

    @Test(expected = Exception.class)
    public void createComplaintAndQNNullTest(){
        complaints.setId(null);
        Mockito.when(manageComplaintService.createComplaintAutomatic((QualityNotifications) qualityNotificationDetails)).thenReturn(complaints);
        handler.createComplaintAndQN((QualityNotifications) qualityNotificationDetails);
    }

    @Test(expected =Exception.class)
    public void createComplaintAndQNullTest(){
        complaints.setId(null);
        Mockito.when(manageComplaintService.createComplaintAutomatic((QualityNotifications) qualityNotificationDetails)).thenReturn(null);
        handler.createComplaintAndQN((QualityNotifications) qualityNotificationDetails);
    }
    
    @Test
    public void createComplaintAndQN(){
        complaints.setId("22222");
        QualityNotifications manageQualityNotifications = Struct.create(QualityNotifications.class);
        manageQualityNotifications.setMaterialCode("BATTERY");
        manageQualityNotifications.setSupplierCode("SUPPLIER");
        Mockito.when(manageComplaintService.createComplaintAutomatic(manageQualityNotifications)).thenReturn(complaints);
        handler.createComplaintAndQN(manageQualityNotifications);
    }


    @Test
    public void checkIfBOCodeEmptyExists(){
        handler.checkIfBOExists("test","");
    }

    @Test
    public void checkIfBOExistsQN(){
    	when(qualityNotificationService.checkIfQNExistsBasedOnNumber("1111")).thenReturn("1");
        handler.checkIfBOExists("1111","QN");
    }
    
    @Test
    public void checkIfBOExistsClaim(){
    	when(internalClaimService.checkIfClaimExistsBasedOnNumber("1111")).thenReturn("1");
        handler.checkIfBOExists("1111","CLM");
    }
    
    @Test
    public void checkIfBOExistsRPO(){
    	when(returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber("1111")).thenReturn("1");
        handler.checkIfBOExists("1111","RPO");
    }

    @Test
    public void testFetchConfiguredDestinationNull() {
        when(configurationDao.getPlantDataBasedOnPlant(any(String.class))).thenReturn(result);
        Plants comp = Struct.create(Plants.class);
        comp.setId("bp");
        List<Plants> compList = new ArrayList<>();
        compList.add(comp);
        when(result.listOf(Plants.class)).thenReturn(compList);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        handler.fetchConfiguredDestination("1000");

    }

    @Test
    public void testFetchConfiguredDestinationHasValue() {
        when(configurationDao.getPlantDataBasedOnPlant(any(String.class))).thenReturn(result);
        when(destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(any(), any())).thenReturn(result);
        Plants comp = Struct.create(Plants.class);
        comp.setId("bp");
        comp.setCompanyCodeIDId("677");
        List<Plants> compList = new ArrayList<>();
        compList.add(comp);
        List<Row> rowvalues = new ArrayList<>();
        row.put("destination", "destination");
        opt = Optional.of(row);
        rowvalues.add(row);
        Mockito.when(result.first()).thenReturn(opt);
        Mockito.when(result.list()).thenReturn(rowvalues);
        when(result.listOf(Plants.class)).thenReturn(compList);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        when(masterDataService.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(comp);
        handler.fetchConfiguredDestination("1000");

    }

    @Test
    public void testFetchConfiguredPlantNull() {
        when(messages.error(MessageKeys.PLANT_IS_MANDATORY)).thenReturn(msg);
        handler.fetchConfiguredDestination(null);
    }
    
    @Test
    public void testQNPayload() {
    	 Map<String, Object> formQNMap = formQNMap();
    	 handler.formPayloadAndUpdateOdata(formQNMap, "QN");
    }
    
    @Test
    public void testQNPayloadDefectEmpty() {
    	 Map<String, Object> formQNMap = formQNMap();
    	 formQNMap.remove("QMFE");
    	 handler.formPayloadAndUpdateOdata(formQNMap, "QN");
    }
    
    @Test
    public void testQNPayloadStatusEmpty() {
    	 Map<String, Object> formQNMap = formQNMap();
    	 List<Object> defectList=(List<Object>)formQNMap.get("QMFE");
    	 Map<String, Object> defectMap=(Map<String, Object>) defectList.get(0);
    	 defectMap.remove("JEST");
    	 handler.formPayloadAndUpdateOdata(formQNMap, "QN");
    }
    
    @Test
    public void testQNPayloadPersonRespEmpty() {
    	 Map<String, Object> formQNMap = formQNMap();
    	 List<Object> defectList=(List<Object>)formQNMap.get("QMFE");
    	 Map<String, Object> defectMap=(Map<String, Object>) defectList.get(0);
    	 List<Object> statusList=(List<Object>)defectMap.get("JEST");
    	 Map<String, Object> statusMap=(Map<String, Object>) statusList.get(0);
    	 statusMap.remove("IHPA");
    	 handler.formPayloadAndUpdateOdata(formQNMap, "QN");
    }
    
    @Test
    public void testQNPayloadEmpty() {
    	 Map<String, Object> formQNMap = new HashMap<>();
    	 handler.formPayloadAndUpdateOdata(formQNMap, "QN");
    }
    @Test
    public void testClaimPayload() {
    	Map<String, Object> claimMap = new HashMap<>();
    	claimMap.put("CLAIM_NUMBER", "234");
    	claimMap.put("CLAIM_STATUS", "CREATED");
    	when(internalClaimService.checkIfClaimExistsBasedOnNumber("234")).thenReturn("122");
   	    handler.formPayloadAndUpdateOdata(claimMap, "CLM");
    }
    @Test
    public void testClaimPayloadBOIdEmpty() {
    	Map<String, Object> claimMap = new HashMap<>();
    	claimMap.put("CLAIM_NUMBER", "234");
    	claimMap.put("CLAIM_STATUS", "");
    	when(internalClaimService.checkIfClaimExistsBasedOnNumber("234")).thenReturn(null);
   	    handler.formPayloadAndUpdateOdata(claimMap, "CLM");
    }
    @Test
    public void testClaimPayloadEmpty() {
    	Map<String, Object> claimMap = new HashMap<>();
   	    handler.formPayloadAndUpdateOdata(claimMap, "CLM");
    }
    @Test
    public void testRPOPayload() {
    	ReturnPurchaseOrderRequestPayload rpoMessagePayload = new ReturnPurchaseOrderRequestPayload();
    	rpoMessagePayload.setRpoNumber("4500000555");
    	List<ReturnOrderBackendStatus> rpoBackendStatusList = new ArrayList<ReturnOrderBackendStatus>();
    	ReturnOrderBackendStatus backendStatus = new ReturnOrderBackendStatus();
    	backendStatus.setStatusCode("IGI");
    	rpoBackendStatusList.add(backendStatus);
    	rpoMessagePayload.setRpoBackendStatusList(rpoBackendStatusList);
    	ObjectMapper oMapper = new ObjectMapper();
    	Map<String, Object> map = oMapper.convertValue(rpoMessagePayload, Map.class);
    	when(returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber("4500000555")).thenReturn("22");
    	handler.formPayloadAndUpdateOdata(map, "RPO");
    }
    
    @Test
    public void testRPOPayloadBOIdEmpty() {
    	ReturnPurchaseOrderRequestPayload rpoMessagePayload = new ReturnPurchaseOrderRequestPayload();
    	rpoMessagePayload.setRpoNumber("4500000555");
    	List<ReturnOrderBackendStatus> rpoBackendStatusList = new ArrayList<ReturnOrderBackendStatus>();
    	ReturnOrderBackendStatus backendStatus = new ReturnOrderBackendStatus();
    	backendStatus.setStatusCode("IGI");
    	rpoBackendStatusList.add(backendStatus);
    	rpoMessagePayload.setRpoBackendStatusList(rpoBackendStatusList);
    	ObjectMapper oMapper = new ObjectMapper();
    	Map<String, Object> map = oMapper.convertValue(rpoMessagePayload, Map.class);
    	when(returnPurchaseOrderService.checkIfReturnPOExistsBasedOnNumber("4500000555")).thenReturn(null);
    	handler.formPayloadAndUpdateOdata(map, "RPO");
    }
    
    @Test
    public void testRPOPayloadEmpty() {
    	Map<String, Object> rpoMap = new HashMap<>();
   	    handler.formPayloadAndUpdateOdata(rpoMap, "RPO");
    }
    
    
   

	public Map<String, Object> formQNMap() {
		Map<String, Object> bmap = new HashMap<>();
         bmap.put("ERNAM","X");
         List<Object> blist = new ArrayList<>();
         blist.add(bmap);
         Map<String, Object> aamap = new HashMap<>();
         aamap.put("INACT","X");
         aamap.put("STAT","234");
         aamap.put("CHGNR","alist");
         aamap.put("IHPA",blist);
         List<Object> aalist = new ArrayList<>();
         aalist.add(aamap);
         Map<String, Object> amap = new HashMap<>();
         amap.put("FEGRP","alist");
         amap.put("FECOD","234");
         amap.put("FENUM","alist");
         amap.put("FETXT","MATNR");
         amap.put("JEST",aalist);
         List<Object> alist = new ArrayList<>();
         alist.add(amap);
         Map<String, Object> map = new HashMap<>();
         map.put("QMFE",alist);
         map.put("QMNUM","234");
         map.put("QMART","alist");
         map.put("MATNR","MATNR");
         map.put("MAWERK","MAWERK");
         map.put("EKORG","EKORG");
         map.put("MGFRD",new Double(4564));
         map.put("MGEIN","MGEIN");
         map.put("LIFNUM","LIFNUM");
         map.put("ERNAM","alist");
         map.put("EBELN","MATNR");
         map.put("EBELP","MAWERK");
         map.put("REFNUM","EKORG");
		return map;
	}
}