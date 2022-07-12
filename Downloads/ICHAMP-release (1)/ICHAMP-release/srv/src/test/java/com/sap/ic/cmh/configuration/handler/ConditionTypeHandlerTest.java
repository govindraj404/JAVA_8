package com.sap.ic.cmh.configuration.handler;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.*;

import cds.gen.configurationservice.BusinessObjectConfigurations;
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
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.ConditionTypeValidation;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ConditionTypes;

public class ConditionTypeHandlerTest {
	@InjectMocks
	ConditionTypeHandler handler;

	@Mock
	ConditionTypeDao conditionTypeDao;
	@Mock
	private AuditLogHelper auditLogHelper;
	@Mock
	ConditionTypeValidation validator;

	@Mock
	Messages messages;

	@Mock
	CdsCreateEventContext createContextMock;

	@Mock
	CqnInsert cqnInsert;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	Result result;
	@Mock
	LoggerHelper loggerHelper;
	@Mock
	CqnAnalyzerUtil cqnAnalyzerUtil;
	@Mock
	CdsDeleteEventContext cdsDeleteEventContext;

    @Mock
    ConfigurationService configurationService;

    @Mock 
    private AuditLogDifference auditLogDifference;
	private ConditionTypes conditionTypes;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		conditionTypes = Struct.create(ConditionTypes.class);
		conditionTypes.setId("100");
		conditionTypes.setConditionType("damg");
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void beforeCreateConditionTypesTest() {
		handler.beforeConditionTypesCreateUpdate(conditionTypes);
	}

	@Test
	public void onCreateConditionTypesTest() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
		handler.onCreateConditionTypes(conditionTypes);
	}
	
	@Test
	public void onCreateConditionTypesTestEmpty() {
		conditionTypes.setIdentifier(10);
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
		handler.onCreateConditionTypes(conditionTypes);
	}
	@Test
	public void afterConditionTypeCreateUpdateTest() {
		List<ConditionTypes> claimStatusMappingsList=new ArrayList<>();
		claimStatusMappingsList.add(conditionTypes);
		//handler.afterConditionTypeCreateUpdate(claimStatusMappingsList);
	}
	 @Test
	 public void afterConditionTypeDeletionTest() {
		 Map<String, Object> targetKeys = new HashMap<>();
		 targetKeys.put("ID", "ConditionType");
		 when(cqnAnalyzerUtil.provideTargetKeys(Mockito.any())).thenReturn(targetKeys);
		 //doNothing().when(auditLogHelper).logDeleteAuditData("1","ConditionType");
		 handler.afterConditionTypeDeletion(cdsDeleteEventContext);
	 }

	@Test
	public void onCreateConditionTypesElseTest() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
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
		handler.onCreateConditionTypes(conditionTypes);
	}

	@Test
	public void onCreateConditionTypesElse1Test() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
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
		handler.onCreateConditionTypes(conditionTypes);
	}

	@Test
	public void onCreateConditionTypesElseNullTest() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
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
		handler.onCreateConditionTypes(conditionTypes);
	}

	@Test(expected = Exception.class)
	public void onCreateConditionTypesElse2Test() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
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
		handler.onCreateConditionTypes(conditionTypes);
	}

	@Test(expected = Exception.class)
	public void onCreateConditionTypesElse3Test() {
		when(conditionTypeDao.getConditionTypes()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(null);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateConditionTypes(conditionTypes);
	}

    @Test
    public void testLogUpsert() {
        
        ConditionTypes mockedObj = ConditionTypes.create();
        mockedObj.setId("test-id");
        when(configurationService.getConditionTypeDetail(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        ConditionTypes conditionTypes = ConditionTypes.create();
        conditionTypes.setId("123");

        ConditionTypes mockAction = mock(ConditionTypes.class);
        when(configurationService.getConditionTypeDetail(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(conditionTypes));
    }
    @Test
    public void testLogDelete() {
        Mockito.when(configurationService.getConditionTypeDetail(anyString()))
                .thenReturn(ConditionTypes.create());
        assertDoesNotThrow(() -> handler.logDelete("test-id"));
    }

    @Test
    public void testBeforeConditionTypeUpdate(){
                when(configurationService.getConditionTypeDetail(conditionTypes.getId())).thenReturn(conditionTypes);
         assertDoesNotThrow(
                () -> handler.beforeConditionTypeUpdate(conditionTypes));
    }

    @Test
    public void testAfterConditionTypeCreateUpdate(){
                when(configurationService.getConditionTypeDetail(conditionTypes.getId())).thenReturn(conditionTypes);
         assertDoesNotThrow(
                () -> handler.afterConditionTypeCreateUpdate(conditionTypes));
    }


}
