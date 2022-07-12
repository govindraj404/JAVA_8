package com.sap.ic.cmh.configuration.handler;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.*;

import cds.gen.configurationservice.ConditionTypes;
import com.sap.cds.Row;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationProperties;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.DestinationConfigurationValidation;
import com.sap.ic.cmh.network.service.DestinationService;

import cds.gen.configurationservice.DestinationConfigurations;
import io.vavr.control.Try;

public class DestinationConfigurationHandlerTest {
	@InjectMocks
	DestinationConfigurationHandler handler;
	@Mock
	DestinationConfigurationDao destinationConfigurationDao;
	@Mock
	DestinationConfigurationValidation validator;
	@Mock
	Messages messages;
	@Mock
	DestinationService destinationService;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	ScpCfDestination scpCfDestination;
	@Mock
	ScpCfDestinationLoader scpCfDestinationLoader;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	private AuditLogHelper auditLogHelper;
	@Mock
	Result result;

	@Mock
	CdsReadEventContext cdsReadEventContext;
	@Mock
	DestinationProperties destinationProperties;
	@Mock
	CqnAnalyzerUtil cqnAnalyzerUtil;
	@Mock
	CdsDeleteEventContext cdsDeleteEventContext;


    @Mock
    ConfigurationService configurationService;

    @Mock 
    private AuditLogDifference auditLogDifference;

	private DestinationConfigurations destinationConfigurations;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		destinationConfigurations = Struct.create(DestinationConfigurations.class);
		destinationConfigurations.setId("101");
		destinationConfigurations.setDestination("nag");
		destinationConfigurations.setCompanyCodeId("Fp01");
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void testBeforeCreateDestinationConfigurationTest() {
		handler.beforeDestinationConfigurationCreateUpdate(destinationConfigurations);
	}

	@Test
	public void testOnCreateDestinationConfigurationTest() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}
	
	@Test
	public void testOnCreateDestinationConfigurationTestEmpty() {
		destinationConfigurations.setIdentifier(2);
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}

	@Test
	public void testFetchAllDestinationsTest() {
		ArrayList<ScpCfDestination> listScpDestination = new ArrayList<>();
		listScpDestination.add(scpCfDestination);
		Iterable<ScpCfDestination> itrIterable = listScpDestination;
		Try<Iterable<ScpCfDestination>> itr = Try.success(itrIterable);
		when(destinationService.getallDestination(any(ScpCfDestinationLoader.class))).thenReturn(itr);
		handler.afterDestinationConfigurationRead(cdsReadEventContext);
	}

	// @Test
	// public void afterDestinationConfigurationCreateUpdateTest() {
	// 	List<DestinationConfigurations> claimStatusMappingsList=new ArrayList<>();
	// 	claimStatusMappingsList.add(destinationConfigurations);
	// 	handler.afterDestinationConfigurationCreateUpdate(claimStatusMappingsList);
	// }
	 @Test
	 public void afterDestinationConfigurationsDeletionTest() {
		 Map<String, Object> targetKeys = new HashMap<>();
		 targetKeys.put("ID", "ConditionType");
		 when(cqnAnalyzerUtil.provideTargetKeys(Mockito.any())).thenReturn(targetKeys);
		 //doNothing().when(auditLogHelper).logDeleteAuditData("1","ConditionType");
	 	handler.afterDestinationConfigurationsDeletion(cdsDeleteEventContext);
	 }

	@Test
	public void testOnCreateDestinationConfigurationElseTest() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("identifier", "2");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}


	@Test
	public void testOnCreateDestinationConfigurationElse1Test() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		//row.put("ID", "201");
		//row.put("identifier", "2");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}
	@Test
	public void testOnCreateDestinationConfigurationNullTest() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("identifier", null);
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}

	@Test(expected = Exception.class)
	public void testOnCreateDestinationConfigurationElse2Test() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("identifier", "test");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}

	@Test(expected = Exception.class)
	public void testOnCreateDestinationConfigurationElse3Test() {
		when(destinationConfigurationDao.getDestinationConfiguration()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(null);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateDestinationConfiguration(destinationConfigurations);
	}

    @Test
    public void testLogUpsert() {
        
        DestinationConfigurations mockedObj = DestinationConfigurations.create();
        mockedObj.setId("test-id");
        when(configurationService.getDestinationConfigDetails(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        DestinationConfigurations destinationConfigurations = DestinationConfigurations.create();
        destinationConfigurations.setId("123");

        DestinationConfigurations mockAction = mock(DestinationConfigurations.class);
        when(configurationService.getDestinationConfigDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(destinationConfigurations));
    }

    @Test
    public void testLogDelete() {
        Mockito.when(configurationService.getDestinationConfigDetails(anyString()))
                .thenReturn(DestinationConfigurations.create());
        assertDoesNotThrow(() -> handler.logDelete("test-id"));
    }

    @Test
    public void testBeforeDestinationConfigurationUpdate(){
                when(configurationService.getDestinationConfigDetails(destinationConfigurations.getId())).thenReturn(destinationConfigurations);
         assertDoesNotThrow(
                () -> handler.beforeDestinationConfigurationUpdate(destinationConfigurations));
    }

    @Test
    public void testAfterDestinationConfigurationCreateUpdate(){
                when(configurationService.getDestinationConfigDetails(destinationConfigurations.getId())).thenReturn(destinationConfigurations);
         assertDoesNotThrow(
                () -> handler.afterDestinationConfigurationCreateUpdate(destinationConfigurations));
    }


}
