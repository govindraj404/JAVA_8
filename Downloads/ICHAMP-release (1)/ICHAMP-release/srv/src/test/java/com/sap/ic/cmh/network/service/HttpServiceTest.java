package com.sap.ic.cmh.network.service;

import cds.gen.claimservice.Claims;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cloud.sdk.cloudplatform.connectivity.Destination;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.RfcDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.cloud.sdk.testutil.MockDestination;
import com.sap.cloud.sdk.testutil.MockUtil;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.model.binary_relation.DocumentFlow;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationResponseFromBapi;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import io.vavr.control.Option;
import io.vavr.control.Try;
import junit.framework.Assert;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.net.URI;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class HttpServiceTest {

	@InjectMocks
	HttpService httpService;
	@Mock
	Messages messages;
	@Mock
	Message msg;
	@Mock
	HttpDestination httpDestination;
	@Mock
	Try<Destination> destinationTry;
	@Mock
	Destination destination;
	@Mock
	RfcDestination rfcDestination;
	@Mock
	Option<Object> option;
	@Mock
	QualityNotificationResponseFromBapi quality;
	@Mock
	Object value;
	@Mock
	Logger logger;
	@Mock
	ScpCfDestinationLoader destinationLoader;
	@Mock
	DestinationService destinationService;
	@Mock
	LoggerHelper loggerHelper;
	@Mock
	HttpClient httpClient;
	@Mock
	HttpResponse response;
	@Mock
	HttpEntity entity;
	@Mock
	ObjectMapper objectMapper;
	@Mock
	CommonFunctions commonFunctions;
	@Mock
	DocumentFlow   flow;
	@Mock
	BinaryRelationObject object;

	@Mock
	CloseableHttpClient  client;
	@Mock
	CloseableHttpResponse closeableHttpResponse;
	@Spy
	LocaleMessageHelper localeMessageHelper=new LocaleMessageHelper();

	private Claims claim;
	private Map<String, Object> claimMap = new HashMap<>();
	private final BinaryRelationObject objectA = new BinaryRelationObject();
	private final BinaryRelationObject objectB = new BinaryRelationObject();

	@Before
	public void beforeClass() {

		MockitoAnnotations.openMocks(this);
		final MockUtil mockUtil = new MockUtil();
		Logger log = LoggerFactory.getLogger(HttpService.class);
		MockDestination destination = MockDestination.builder("CPIintegration", URI.create("http:localhost")).build();

		mockUtil.mockDestination(destination);

		claim = Struct.create(Claims.class);
		claim.setId(UUID.randomUUID().toString());
		ObjectMapper oMapper = new ObjectMapper();
		claimMap = oMapper.convertValue(claim, Map.class);

		objectA.setObjectKey("000200002341");
		objectA.setObjectType(Constants.QUALITY_NOTIFICATION_OBJECT_TYPE);
		objectA.setLogSys("QM7CLNT910");
		objectB.setObjectKey("000003982");
		objectB.setObjectType(Constants.CLAIM_OBJECT_TYPE);
		objectB.setLogSys("QM7CLNT910");
	}

	@Test
	public void testCallCpiFlow() throws IOException {
		ResponseModel responseModel=new ResponseModel();
		responseModel.setResult("000012");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("I0068");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"result\":\"000012\",\"errorMessage\":\"-\",\"statusCode\":\"I0068\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		ser1.callCpiFlow("/appendedUrl", claimMap, "");
		Assert.assertEquals(responseModel.getErrorMessage(), "");
		Assert.assertEquals(responseModel.getStatusCode(), "I0068");
		Assert.assertEquals(responseModel.getResult(), "000012");
	}

	@Test
	public void testGetLogicalSystem() {
		when(destinationService.getHttpDestination(any(), any())).thenReturn(httpDestination);
		;
		when(httpDestination.get("description")).thenReturn(option);
		when(option.get()).thenReturn(value);
		httpService.getLogicalSystem(destinationLoader, "bapdest");
	}

	@Test
	public void testGetTargetLogicalSystem() {
		when(destinationService.getRfcDestination(any(), any())).thenReturn(rfcDestination);
		when(rfcDestination.get("jco.destination.description")).thenReturn(option);
		when(option.get()).thenReturn(value);
		httpService.getTargetLogicalSystem(destinationLoader, "bapdest");
	}

	@Test
	public void testSetRequestForDocumentFlow() {
		ObjectMapper oMapper = new ObjectMapper();
		httpService.setRequestForDocumentFlow(objectA, objectB, oMapper);
	}

	@Test
	public void tesCreateDocumentFlow() throws IOException {
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"errorMessage\":\"\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		ser1.createDocumentFlow(objectA, objectB, "dest");
	}

	@Test(expected = ServiceException.class)
	public void tesCreateDocumentFlowError() throws IOException {
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"error\":\"\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		ser1.createDocumentFlow(objectA, objectB, "dest");
	}

	@Test(expected=NullPointerException.class)
	public void testCreateDocumentFlowObjects() throws IOException {
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"errorMessage\":\"\"}";
		Mockito.doReturn("logicalsys").when(ser1).getLogicalSystem(any(ScpCfDestinationLoader.class),
				any(String.class));
		when(commonFunctions.setDocumentFlowRequest("oba","obta", "obb", "obtb", "logicalsys","logicalsys")).thenReturn(setRequest());

		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		ser1.createDocumentFlowObjects(destinationLoader, "oba", "obta", "obb", "obtb", "dest","dest1");
	}

	@Test(expected = NullPointerException.class)
	public void testCreateDocumentFlowObjectsException() throws IOException {
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"errorMessage\":\"\"}";
		Mockito.doReturn("logicalsys").when(ser1).getLogicalSystem(any(ScpCfDestinationLoader.class),
				any(String.class));
		when(commonFunctions.setDocumentFlowRequest("oba","obta", "obb", "obtb", "logicalsys","logicalsys")).thenReturn(new DocumentFlow());
		Mockito.doThrow(IOException.class).when(ser1).executeHttpPost(any(String.class), any(Map.class),
				any(String.class), any(ObjectMapper.class));
		ser1.createDocumentFlowObjects(destinationLoader, "oba", "obta", "obb", "obtb", "dest","dest1");
	}

	@Test(expected = Exception.class)
	public void testGetLogicalSystemNoSuchElementException() {
		when(destinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		when(destinationTry.get()).thenReturn(destination);
		when(destination.asHttp()).thenReturn(httpDestination);
		when(httpDestination.get("description")).thenReturn(option);
		when(option.get()).thenThrow(NoSuchElementException.class);
		httpService.getLogicalSystem(destinationLoader, "bapdest");
	}

	@Test(expected = Exception.class)
	public void testGetTargetLogicalSystemNoSuchElementException() {
		when(destinationLoader.tryGetDestination(any(), any())).thenReturn(destinationTry);
		when(destinationTry.get()).thenReturn(destination);
		when(destination.asRfc()).thenReturn(rfcDestination);
		when(rfcDestination.get("jco.destination.description")).thenReturn(option);
		when(option.get()).thenThrow(NoSuchElementException.class);
		httpService.getTargetLogicalSystem(destinationLoader, "bapdest");
	}

	@Test(expected =Exception.class)
	public void executeHttpPostTest() throws IOException {
		claim = Struct.create(Claims.class);
		claim.setId(UUID.randomUUID().toString());
		ObjectMapper oMapper = new ObjectMapper();
		claimMap = oMapper.convertValue(claim, Map.class);
		String json="/getClaim";
		Map<String,Object> map=new HashMap<>();
		map.put("notificationType","type1");
		map.put("material","mat1");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response1 = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"errorMessage\":\"\"}";
		when(objectMapper.writeValueAsString(any())).thenReturn(response1);
		when(httpClient.execute(any())).thenReturn(response);
		ser1.executeHttpPost(":8080/getClaim/",claimMap,"//http/getClaim",objectMapper);
	}

	@Test(expected = Exception.class)
	public void testCallCpiFlowElse() throws IOException {
		ResponseModel responseModel=new ResponseModel();
		responseModel.setResult("000012");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("I0068");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"result\":\"\",\"errorMessage\":\"-\",\"statusCode\":\"I0068\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		ser1.callCpiFlow("/appendedUrl", claimMap, "");
	}

	private DocumentFlow setRequest() {
		DocumentFlow docFlow = new DocumentFlow();
		BinaryRelationObject objectA = new BinaryRelationObject();
		BinaryRelationObject objectB = new BinaryRelationObject();
		docFlow.setObjectANumber("oba");
		docFlow.setObjectAType("obta");
		docFlow.setObjectBNumber("obb");
		docFlow.setObjectBType("obtb");
		docFlow.setObjectA(objectA);
		docFlow.setObjectB(objectB);
		docFlow.setSourceLogicalSystem("logicalsys");
		docFlow.setTargetLogicalSystem("logicalsys");
		return docFlow;
	}

	@Test
	public void  setBinaryRelationObjectsTest()  {
		String json="/getClaim";
		Map<String,Object> map=new HashMap<>();
		map.put("notificationType","type1");
		map.put("material","mat1");
		when(flow.getObjectA()).thenReturn(object);
		when(flow.getObjectB()).thenReturn(object);

		httpService. setBinaryRelationObjects(flow);
	}

	@Test(expected = Exception.class)
	public void testCallCpiFlowElse1() throws IOException {
		ResponseModel responseModel=new ResponseModel();
		responseModel.setResult("");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("I0068");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"result\":\"\",\"errorMessage\":\"-\",\"statusCode\":\"I0068\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		String test="test";
		LocaleMessageHelper localeMessageHelper=Mockito.mock(LocaleMessageHelper.class);
		Mockito.when(localeMessageHelper.getMessage(any())).thenReturn(test);
		Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		ser1.callCpiFlow("/appendedUrl", claimMap, "test");

	}

	@Test(expected = Exception.class)
	public void testCreateDocumentFlowObjects1() throws IOException {
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"qualityNotificationDetails\":{\"notificationType\":\"type1\",\"material\":\"mat1\"},\"errorMessage\":\"\"}";
		Mockito.doReturn("logicalsys").when(ser1).getLogicalSystem(any(ScpCfDestinationLoader.class),
				any(String.class));

		when(option.get()).thenReturn(option);
		when(httpDestination.get(any(String.class))).thenReturn(option);
		when(destinationService.getHttpDestination(any(),any())).thenReturn(httpDestination);
		when(commonFunctions.setDocumentFlowRequest(any(),any(),any(),any(),any(),any())).thenReturn(flow);
		when(flow.getObjectA()).thenReturn(object);
		when(flow.getObjectB()).thenReturn(object);
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		httpService.createDocumentFlowObjects(destinationLoader, "oba", "obta", "obb", "obtb", "dest","dest1");
	}

	@Test(expected = Exception.class)
	public void getTest() throws IOException {
		Map<String,String> map=new HashMap<>();
		map.put("key","test");
		map.put("key1","test2");
		when(client.execute(any())).thenReturn(closeableHttpResponse);
		httpService.get("/getcliam","abdc324823fsdg",map);
	}

	@Test(expected =Exception.class)
	public void testCallCpiFlowElse3() throws IOException {
		ResponseModel responseModel=new ResponseModel();
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("I0068");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"result\":\"\",\"errorMessage\":\"-\",\"statusCode\":\"I0068\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		LocaleMessageHelper localeMessageHelper=new LocaleMessageHelper();
		localeMessageHelper.getMessage("test");
		ser1.callCpiFlow("/appendedUrl", claimMap, "");

	}


	@Test(expected = Exception.class)
	public void testCallCpiFlowElse5() throws IOException {
		ResponseModel responseModel=new ResponseModel();
		responseModel.setResult("000012");
		responseModel.setErrorMessage("");
		responseModel.setStatusCode("I0068");
		HttpService ser = new HttpService();
		HttpService ser1 = Mockito.spy(ser);
		String response = "{\"result\":\"\",\"errorMessage\":\"-\",\"statusCode\":\"I0068\"}";
		Mockito.doReturn(response).when(ser1).executeHttpPost(any(String.class), any(Map.class), any(String.class),
				any(ObjectMapper.class));
		Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		Mockito.when(msg.target(any(String.class))).thenReturn(msg);
		String test="test";
		localeMessageHelper.getMessage("test");
		Mockito.when(localeMessageHelper.getMessage(any(String.class))).thenReturn(test);
		CdsRuntime cdsRuntime=Mockito.mock(CdsRuntime.class);
		when(cdsRuntime.getLocalizedMessage(any(String.class),any(Object[].class),any(Locale.class))).thenReturn(test);

		ser1.callCpiFlow("/appendedUrl", claimMap, "");
	}

}