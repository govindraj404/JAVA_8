package com.sap.ic.cmh.configuration.handler;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.*;

import cds.gen.configurationservice.DestinationConfigurations;
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
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.ServiceMaterialValidation;
import com.sap.ic.cmh.configuration.validations.ServiceMaterialUnitValidation;
import cds.gen.configurationservice.ServiceMaterials;

public class ServiceMaterialHandlerTest {
	@InjectMocks
	ServiceMaterialHandler handler;
	@Mock
	ServiceMaterialDao serviceMaterialDao;
    @Mock
    ServiceMaterialValidation serviceMaterialValidator;
    @Mock
	ServiceMaterialUnitValidation serviceMaterialUnitValidator;
	@Mock
	Messages messages;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	private AuditLogHelper auditLogHelper;
	@Mock
	protected PersistenceService mockDb;
	@Mock
	Result result;
	@Mock
	CqnAnalyzerUtil cqnAnalyzerUtil;
	@Mock
	CdsDeleteEventContext cdsDeleteEventContext;

    @Mock
    ConfigurationService configurationService;

    @Mock 
    private AuditLogDifference auditLogDifference;

	private ServiceMaterials serviceMaterials;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		serviceMaterials = Struct.create(ServiceMaterials.class);
		serviceMaterials.setId("100");
		serviceMaterials.setServiceMaterial("damg");
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void beforeCreateServiceMaterialTest() {
		handler.beforeServiceMaterialCreateUpdate(serviceMaterials);
	}

	@Test
	public void onCreateServiceMaterialTest() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
		handler.onCreateServiceMaterial(serviceMaterials);
	}
	
	@Test
	public void onCreateServiceMaterialTestNotEmpty() {
		serviceMaterials.setIdentifier(2);
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
		handler.onCreateServiceMaterial(serviceMaterials);
	}

	// @Test
	// public void afterServiceMaterialCreateUpdateTest() {
	// 	List<ServiceMaterials> claimStatusMappingsList=new ArrayList<>();
	// 	claimStatusMappingsList.add(serviceMaterials);
	// 	handler.afterServiceMaterialCreateUpdate(claimStatusMappingsList);
	// }
	 @Test
	 public void afterServiceMaterialsDeletionTest() {
		 Map<String, Object> targetKeys = new HashMap<>();
		 targetKeys.put("ID", "ConditionType");
		 when(cqnAnalyzerUtil.provideTargetKeys(Mockito.any())).thenReturn(targetKeys);
		 //doNothing().when(auditLogHelper).logDeleteAuditData("1","ConditionType");
	 	handler.afterServiceMaterialsDeletion(cdsDeleteEventContext);
	 }

	@Test
	public void onCreateServiceMaterialElseTest() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
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
		handler.onCreateServiceMaterial(serviceMaterials);
	}

	@Test
	public void onCreateServiceMaterialNullTest() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
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
		handler.onCreateServiceMaterial(serviceMaterials);
	}

	@Test
	public void onCreateServiceMaterialElse1Test() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		//row.put("ID", "201");
		//row.put("identifier", "test");
		Optional<Row> opt = Optional.of(row);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(opt);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateServiceMaterial(serviceMaterials);
	}

	@Test(expected = Exception.class)
	public void onCreateServiceMaterialElseExpTest() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
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
		handler.onCreateServiceMaterial(serviceMaterials);
	}

	@Test(expected = Exception.class)
	public void onCreateServiceMaterialNullExpTest() {
		when(serviceMaterialDao.getServiceMaterials()).thenReturn(result);
		List<Row> rowValues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		rowValues.add(row);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowValues);
		when(result.first()).thenReturn(null);
		when(result.iterator()).thenReturn(rowValues.iterator());
		handler.onCreateServiceMaterial(serviceMaterials);
	}


    @Test
    public void testLogUpsert() {
        
        ServiceMaterials mockedObj = ServiceMaterials.create();
        mockedObj.setId("test-id");
        when(configurationService.getServiceMaterialsDetails(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        ServiceMaterials serviceMaterials = ServiceMaterials.create();
        serviceMaterials.setId("123");

        ServiceMaterials mockAction = mock(ServiceMaterials.class);
        when(configurationService.getServiceMaterialsDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(serviceMaterials));
    }

    @Test
    public void testLogDelete() {
        Mockito.when(configurationService.getServiceMaterialsDetails(anyString()))
                .thenReturn(ServiceMaterials.create());
        assertDoesNotThrow(() -> handler.logDelete("test-id"));
    }

    @Test
    public void testBeforeServiceMaterialUpdate(){
                when(configurationService.getServiceMaterialsDetails(serviceMaterials.getId())).thenReturn(serviceMaterials);
         assertDoesNotThrow(
                () -> handler.beforeServiceMaterialUpdate(serviceMaterials));
    }

    @Test
    public void testAfterServiceMaterialCreateUpdate(){
                when(configurationService.getServiceMaterialsDetails(serviceMaterials.getId())).thenReturn(serviceMaterials);
         assertDoesNotThrow(
                () -> handler.afterServiceMaterialCreateUpdate(serviceMaterials));
    }


}