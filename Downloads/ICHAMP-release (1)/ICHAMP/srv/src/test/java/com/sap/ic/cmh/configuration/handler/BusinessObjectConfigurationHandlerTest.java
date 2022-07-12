package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.BusinessObjectConfigurationValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class BusinessObjectConfigurationHandlerTest {
	@InjectMocks
	BusinessObjectConfigurationHandler handler;
	@Mock
	BusinessObjectConfigurationDao businessObjectConfigurationDao;
	@Mock
	BusinessObjectConfigurationValidation validator;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CdsDeleteEventContext contextDeleteEventContext1;
	@Mock
	private AuditLogHelper auditLogHelper;
	private Row row;
	private Optional<Row> opt;

	@Mock
	CqnInsert cqnInsert;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	Result result;
	@Mock
	Messages messages;
	@Mock
	CqnAnalyzerUtil cqnAnalyzerUtil;
	private BusinessObjectConfigurations businessObjectConfigurations;

    @Mock
    ConfigurationService configurationService;

    @Mock 
    private AuditLogDifference auditLogDifference;


	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		businessObjectConfigurations = Struct.create(BusinessObjectConfigurations.class);
		businessObjectConfigurations.setId("100");
		businessObjectConfigurations.setDestination("GNT");
		businessObjectConfigurations.setIdentifier(10);
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void beforeCreateBusinessObjectConfigurationTest() {
		doNothing().when(validator).validateBusinessObjectConfiguration(businessObjectConfigurations);
		handler.beforeBusinessObjectConfigurationCreateUpdate(businessObjectConfigurations);
	}

	@Test
	public void onCreateBusinessObjectConfigurationTest() {
		List<Row> rowvalues = new ArrayList<>();
		row = Struct.create(Row.class);
		row.put("businessObjectValue", "alist");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(businessObjectConfigurationDao.getBusinessObjectConfigBasedOnDestinationAndBO(any(String.class),
				any(String.class), any(String.class), any(String.class))).thenReturn(result);
		when(result.first()).thenReturn(opt);
		Mockito.when(result.list()).thenReturn(rowvalues);
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);
	}

	@Test
	public void onCreateBusinessObjectConfigurationTestEmpty() {
		businessObjectConfigurations.setIdentifier(null);
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);
	}
	// @Test
	// public void afterBusinessObjectCreateUpdateTest() {
    //     doNothing().when(handler.logUpsert(any()));
	// 	handler.afterBusinessObjectCreateUpdate(businessObjectConfigurations);
	// }


	@Test
	public void afterBusinessObjectDeletionTest() {
		Map<String, Object> targetKeys = new HashMap<String, Object>();
		targetKeys.put("ID", "BO");
		Mockito.when(cqnAnalyzerUtil.provideTargetKeys(Mockito.any())).thenReturn(targetKeys);
		//Mockito.doNothing().when(auditLogHelper).logDeleteAuditData("1","BO");
		handler.afterBusinessObjectDeletion(contextDeleteEventContext1);
	}


	@Test
	public void onCreateBusinessObjectConfigurationElseTest() {
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
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
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);

	}

	@Test
	public void onCreateBusinessObjectConfigurationElse1Test() {
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		//row.put("ID", "201");
		//row.put("identifier", "2");
		Optional<Row> opt = Optional.of(row);
		//rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(null);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);

	}

	@Test(expected = Exception.class)
	public void onCreateBusinessObjectConfigurationNullTest() {
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		Optional<Row> opt = Optional.of(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(null);
		when(result.first()).thenReturn(null);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);

	}

	@Test
	public void onCreateBusinessObjectConfigurationNull1Test() {
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
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
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);

	}

	@Test(expected = Exception.class)
	public void onCreateBusinessObjectConfigurationNull2Test() {
		when(businessObjectConfigurationDao.getBusinessObjectConfigurations()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "201");
		row.put("identifier", "null");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateBusinessObjectConfiguration(businessObjectConfigurations);

	}

    @Test
    public void testLogUpsert() {
        
        BusinessObjectConfigurations mockedObj = BusinessObjectConfigurations.create();
        mockedObj.setId("test-id");
        when(configurationService.getBusinessObjectConfigurationsDetails(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        BusinessObjectConfigurations businessObjectConfigurations = BusinessObjectConfigurations.create();
        businessObjectConfigurations.setId("123");

        BusinessObjectConfigurations mockAction = mock(BusinessObjectConfigurations.class);
        when(configurationService.getBusinessObjectConfigurationsDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(businessObjectConfigurations));
    }

    @Test
    public void testLogDelete() {
        Mockito.when(configurationService.getBusinessObjectConfigurationsDetails(anyString()))
                .thenReturn(BusinessObjectConfigurations.create());
        assertDoesNotThrow(() -> handler.logDelete("test-id"));
    }

    @Test
    public void testAfterBusinessObjectCreateUpdate(){
                when(configurationService.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId())).thenReturn(businessObjectConfigurations);
         assertDoesNotThrow(
                () -> handler.afterBusinessObjectCreateUpdate(businessObjectConfigurations));
    }

    @Test
    public void testBeforeBusinessObjectCreateUpdate(){
                when(configurationService.getBusinessObjectConfigurationsDetails(businessObjectConfigurations.getId())).thenReturn(businessObjectConfigurations);
         assertDoesNotThrow(
                () -> handler.beforeBusinessObjectCreateUpdate(businessObjectConfigurations));
    }


}
