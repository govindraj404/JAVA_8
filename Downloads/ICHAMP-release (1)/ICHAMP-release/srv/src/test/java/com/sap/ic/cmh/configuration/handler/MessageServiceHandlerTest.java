package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ServiceMaterials;
import com.sap.ic.cmh.configuration.handler.MessageServiceHandler;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.EventContext;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messaging.TopicMessageEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.RequestContext;
import com.sap.cds.services.request.UserInfo;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cds.services.runtime.RequestContextRunner;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.service.MessageService;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDetails;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import com.sap.ic.cmh.utils.QnValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.model.SaasSubscription;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class MessageServiceHandlerTest {
	@InjectMocks
	MessageServiceHandler handler;
	@Mock
	PersistenceService db;
	@Mock
	CdsRuntime cdstime;
	@Mock
	BusinessObjectConfigurationDao businessObjectConfigurationDao;
	@Mock
	DestinationService destinationService;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	PersistenceService mockDb;
	@Mock
	Result result;
	@Mock
	UserInfo userInfo;

	@Mock
	EventContext eventContext;
	@Mock
	TopicMessageEventContext context;

	@Mock
	QualityNotificationService qualityNotificationService;

	@Mock
	QnValidation qnValidation;

	@Mock
	HttpService httpservice;

	private Row row;
	private Optional<Row> opt;

	@Mock
	MessageService messageService;

	@Mock
	ObjectMapper mapper;
	@Mock
	ReturnPurchaseOrderService returnPurchaseOrderService;
	@Mock
	RequestContextRunner runner1;
	@Mock
	TenantSubscriptionService tenantSubscriptionService;
	@Mock
	SaasSubscription saasSubscription;
	private ServiceMaterials serviceMaterials;
	List<TopicMessageEventContext> topicMessageEventContext;
	String json;
	RequestContext context1;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		row = Struct.create(Row.class);
		context1 = Struct.create(RequestContext.class);
		context = Struct.create(TopicMessageEventContext.class);
		json = "{\"data\":[{\"claim_notification\":4}]}";
		context.setData(json);
		context.setMessageId("damg");
		context.setIsInbound(true);
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void testOnAutomaticQNCreate() {
		ConcurrentMap<String, String> qnNumberMap = new ConcurrentHashMap<String, String>();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F3\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"X\",\"CHGNR\":\"001\",\"IHPA\":[{\"ERNAM\":\"test\"}]},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"X\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(context.getData()).thenReturn(json);
		when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("");
		when(messageService.fetchConfiguredDestination(any(String.class))).thenReturn("destination");
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);

		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.list()).thenReturn(rowvalues);
		Mockito.when(result.first()).thenReturn(opt);

		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticQNCreate(context);

		}

	}

	@Test
	public void testOnAutomaticQNCreateException() {
		ConcurrentMap<String, String> qnNumberMap = new ConcurrentHashMap<String, String>();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F3\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"X\",\"CHGNR\":\"001\",\"IHPA\":[{\"ERNAM\":\"test\"}]},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"X\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(context.getData()).thenReturn(json);
		when(qnValidation.getQnMap()).thenReturn(qnNumberMap);
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("");
		when(messageService.fetchConfiguredDestination(any(String.class))).thenThrow(NullPointerException.class);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);

		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.list()).thenReturn(rowvalues);
		Mockito.when(result.first()).thenReturn(opt);

		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticQNCreate(context);

		}

	}

	@Test
	public void testOnAutomaticQNUpdateTrue() {
		QualityNotificationDetails qualityNotificationDetails = new QualityNotificationDetails();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("2345");
		when(messageService.fetchConfiguredDestination(any())).thenReturn("destination");
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.list()).thenReturn(rowvalues);
		Mockito.when(result.first()).thenReturn(opt);

		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticQNUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticQNUpdateFalse() {
		QualityNotificationDetails qualityNotificationDetails = new QualityNotificationDetails();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("");
		when(messageService.fetchConfiguredDestination(any())).thenReturn("destination");
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		Mockito.when(result.list()).thenReturn(rowvalues);
		Mockito.when(result.first()).thenReturn(opt);

		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticQNUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticQNUpdateODataException() {
		QualityNotificationDetails qualityNotificationDetails = new QualityNotificationDetails();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("2345");
		when(messageService.fetchConfiguredDestination(any())).thenReturn("destination");
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class))).thenThrow(ODataException.class);

				})) {

			handler.onAutomaticQNUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticQNUpdateJsonProcessingException() {
		QualityNotificationDetails qualityNotificationDetails = new QualityNotificationDetails();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("2345");
		when(messageService.fetchConfiguredDestination(any())).thenReturn("destination");

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenThrow(JsonProcessingException.class);

				})) {

			handler.onAutomaticQNUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticQNUpdateTrueException() {
		QualityNotificationDetails qualityNotificationDetails = new QualityNotificationDetails();
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);
			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("2345");
		when(messageService.fetchConfiguredDestination(any())).thenReturn("destination");

		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticQNUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticClaimStatusUpdateTest() {
		String json = "{\n" + "    \"PNWTYH\": [\n" + "      {\n" + "        \"MANDT\": \"100\",\n"
				+ "        \"CLAIM_NUMBER\": \"000000123617\",\n" + "        \"CLAIM_STATUS\": \"B020\",\n"
				+ "        \"PNGUID\": \"FA163EFC9BC31EEC81E1787D7EA64270\",\n" + "        \"PNCNT\": \"00000001\"\n"
				+ "      }\n" + "    ]\n" + "  }";

		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		Subscription subs = mock(Subscription.class);
		saasSubscription = mock(SaasSubscription.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		Map<String, Object> claimMessageMap = new HashMap<>();
		List<Subscription> subdomainlist = new ArrayList<>();
		subdomainlist.add(subs);
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(subs.getConsumerTenantId()).thenReturn("f445676889999");
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);
		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("f445676889999");
		when(tenantSubscriptionService.getSaasSubscription()).thenReturn(saasSubscription);
		when(saasSubscription.getSubscriptions()).thenReturn(subdomainlist);
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("123");
		try {
			doNothing().when(httpservice).callCpiFlowForautoCreation("json", claimMessageMap, "", "json");

		} catch (Exception e) {

		}

		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());

		Map<String, Object> map = new HashMap<>();
		map.put("CLAIM_NUMBER", "234");
		map.put("CLAIM_STATUS", "CREATED");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("PNWTYH", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticClaimStatusUpdate(context);

		}

	}

	@Test
	public void testExecuteQualityNotificationCreateUpdate() {
		String json = "{\n" + "\"specversion\" : \"1.0\",\n"
				+ "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
				+ "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
				+ "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n" + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
				+ "\"data\" :\n"
				+ "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
				+ "}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		Subscription subs = mock(Subscription.class);
		saasSubscription = mock(SaasSubscription.class);
		List<Subscription> subdomainlist = new ArrayList<>();
		subdomainlist.add(subs);
		when(context.getData()).thenReturn(json);
		when(tenantSubscriptionService.getSaasSubscription()).thenReturn(saasSubscription);
		when(saasSubscription.getSubscriptions()).thenReturn(subdomainlist);
		Map<String, Object> bmap = new HashMap<>();
		bmap.put("ERNAM", "X");
		bmap.put("PARVW", "alist");
		List<Object> blist = new ArrayList<>();
		blist.add(bmap);
		Map<String, Object> aamap = new HashMap<>();
		aamap.put("INACT", "X");
		aamap.put("STAT", "234");
		aamap.put("CHGNR", "alist");
		aamap.put("IHPA", blist);
		List<Object> aalist = new ArrayList<>();
		aalist.add(aamap);
		Map<String, Object> amap = new HashMap<>();
		amap.put("FEGRP", "alist");
		amap.put("FECOD", "234");
		amap.put("FENUM", "alist");
		amap.put("FETXT", "MATNR");
		amap.put("JEST", aalist);
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> map = new HashMap<>();
		map.put("QMFE", alist);
		map.put("QMNUM", "234");
		map.put("QMART", "alist");
		map.put("MATNR", "MATNR");
		map.put("MAWERK", "MAWERK");
		map.put("EKORG", "EKORG");
		map.put("MGFRD", new Double(4564));
		map.put("MGEIN", "MGEIN");
		map.put("LIFNUM", "LIFNUM");
		map.put("ERNAM", "alist");
		map.put("EBELN", "MATNR");
		map.put("EBELP", "MAWERK");
		map.put("REFNUM", "EKORG");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("QMEL", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		Map<String, Object> qmelMap = new HashMap<>();
		when(context.getUserInfo()).thenReturn(userInfo);
		when(subs.getConsumerTenantId()).thenReturn("f445676889999");
		when(userInfo.getTenant()).thenReturn("f445676889999");
		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);
		when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(rowvalues);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {
					doNothing().when(httpservice).callCpiFlowForautoCreation("json", qmelMap, "", "json");
					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.executeQualityNotificationCreateUpdate(context);

		}
	}

	@Test
	public void testOnAutomaticClaimStatusUpdateException() {
		String json = "{\n" + "    \"PNWTYH\": [\n" + "      {\n" + "        \"MANDT\": \"100\",\n"
				+ "        \"CLAIM_NUMBER\": \"000000123617\",\n" + "        \"CLAIM_STATUS\": \"B020\",\n"
				+ "        \"PNGUID\": \"FA163EFC9BC31EEC81E1787D7EA64270\",\n" + "        \"PNCNT\": \"00000001\"\n"
				+ "      }\n" + "    ]\n" + "  }";

		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		Map<String, Object> claimMessageMap = new HashMap<>();
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("1683466e-7a50-4d1e-acd4-78ff8ca609dd");
		try {
			doNothing().when(httpservice).callCpiFlowForautoCreation("json", claimMessageMap, "", "json");
		} catch (Exception e) {

		}

		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());

		Map<String, Object> map = new HashMap<>();
		map.put("CLAIM_NUMBER", "234");
		map.put("CLAIM_STATUS", "CREATED");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("PNWTYH", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);
		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);

				})) {

			handler.onAutomaticClaimStatusUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticClaimStatusUpdateJsonProcessingException() {
		String json = "{\n" + "    \"PNWTYH\": [\n" + "      {\n" + "        \"MANDT\": \"100\",\n"
				+ "        \"CLAIM_NUMBER\": \"000000123617\",\n" + "        \"CLAIM_STATUS\": \"B020\",\n"
				+ "        \"PNGUID\": \"FA163EFC9BC31EEC81E1787D7EA64270\",\n" + "        \"PNCNT\": \"00000001\"\n"
				+ "      }\n" + "    ]\n" + "  }";

		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getData()).thenReturn(json);
		when(context.getUserInfo()).thenReturn(userInfo);
		when(userInfo.getTenant()).thenReturn("1683466e-7a50-4d1e-acd4-78ff8ca609dd");
		when(messageService.checkIfBOExists(any(String.class), any(String.class))).thenReturn("123");
		List<Row> rowvalues = new ArrayList<>();
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first()).thenReturn(opt);

		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenThrow(JsonProcessingException.class);

				})) {

			handler.onAutomaticClaimStatusUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticReturnOrderStatusUpdate() {
		String json = "{\"data\":{\"RPO_ID\":\"RPOID/jhuDFRDF/hdfggs\"}}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		Subscription subs = mock(Subscription.class);
		saasSubscription = mock(SaasSubscription.class);
		List<Subscription> subdomainlist = new ArrayList<>();
		subdomainlist.add(subs);
		when(context.getData()).thenReturn(json);
		when(tenantSubscriptionService.getSaasSubscription()).thenReturn(saasSubscription);
		when(saasSubscription.getSubscriptions()).thenReturn(subdomainlist);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getUserInfo()).thenReturn(userInfo);
		when(subs.getConsumerTenantId()).thenReturn("f445676889999");
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());

		Map<String, Object> map = new HashMap<>();
		Map<String, Object> returnPORequestMap = new HashMap<>();
		map.put("RPO_ID", "\"RPO_ID\":\"RPOID/jhuDFRDF/hdfggs\"}");
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("PNWTYH", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", map);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);
					doNothing().when(httpservice).callCpiFlowForautoCreation("json", returnPORequestMap, "", "json");
				})) {

			handler.onAutomaticReturnOrderStatusUpdate(context);

		}

	}

	@Test
	public void testOnAutomaticReturnOrderStatusUpdateWithOtherPayload() {
		String json = "{\"data\":{\"ekko\":\"ekko/jhuDFRDF/hdfggs\"}}";
		TopicMessageEventContext context = mock(TopicMessageEventContext.class);
		Subscription subs = mock(Subscription.class);
		saasSubscription = mock(SaasSubscription.class);
		List<Subscription> subdomainlist = new ArrayList<>();
		subdomainlist.add(subs);
		when(context.getData()).thenReturn(json);
		when(tenantSubscriptionService.getSaasSubscription()).thenReturn(saasSubscription);
		when(saasSubscription.getSubscriptions()).thenReturn(subdomainlist);
		context.setMessageId("234");
		context.setData("sdmfm");
		CdsRuntime cdsRunTime = mock(CdsRuntime.class);
		RequestContextRunner runner = mock(RequestContextRunner.class);
		RequestContextRunner runner1 = mock(RequestContextRunner.class);
		runner.user(userInfo);
		when(context.getCdsRuntime()).thenReturn(cdsRunTime);
		when(cdsRunTime.requestContext()).thenReturn(runner);
		when(runner.modifyUser(any())).thenReturn(runner1);

		when(context.getUserInfo()).thenReturn(userInfo);
		when(subs.getConsumerTenantId()).thenReturn("f445676889999");
		when(userInfo.getTenant()).thenReturn("f445676889999");
		Mockito.doAnswer(invocation -> {
			Consumer<RequestContext> consumer = invocation.getArgument(0);
			consumer.accept(context1);

			return null;
		}).when(runner1).run((Consumer<RequestContext>) Mockito.any());

		Map<String, Object> returnPORequestMap = new HashMap<>();

		Map<String, Object> amap = new HashMap<>();
		amap.put("WERKS", "WERKS");
		List<Object> alist = new ArrayList<>();
		alist.add(amap);
		Map<String, Object> rmap = new HashMap<>();
		rmap.put("COMPLETE", "X");
		rmap.put("MSR_D_EXECUTED", alist);
		List<Object> rpolist = new ArrayList<>();
		rpolist.add(rmap);
		Map<String, Object> map = new HashMap<>();
		map.put("EBELN", "1234");
		map.put("MSR_D_HEAD", rpolist);
		List<Object> list = new ArrayList<>();
		list.add(map);
		Map<String, Object> eventMap = new HashMap<>();
		eventMap.put("EKKO", list);
		Map<String, Object> eventMessagePayloadMap = new HashMap<>();
		eventMessagePayloadMap.put("data", eventMap);

		try (MockedConstruction<ObjectMapper> mocked = Mockito.mockConstruction(ObjectMapper.class,
				(mock, tcontext) -> {

					when(mock.readValue(any(String.class), any(TypeReference.class)))
							.thenReturn(eventMessagePayloadMap);
					doNothing().when(httpservice).callCpiFlowForautoCreation("json", returnPORequestMap, "", "json");
				})) {

			handler.onAutomaticReturnOrderStatusUpdate(context);

		}

	}

}