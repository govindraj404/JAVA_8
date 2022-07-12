package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ClaimStatusMappings;
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
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.validations.ClaimStatusMappingsValidation;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.rmi.server.ExportException;
import java.util.*;

public class ClaimStatusMappingsHandlerTest {
    @InjectMocks
    ClaimStatusMappingsHandler handler;

    @Mock
    ClaimStatusMappingsValidation claimStatusValidation;

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
    private AuditLogHelper auditLogHelper;
    @Mock
    ClaimStatusMappingsDao claimStatusMappingsDao;
    @Mock
    CqnAnalyzerUtil cqnAnalyzerUtil;
    @Mock
    CdsDeleteEventContext cdsDeleteEventContext;

    @Mock
    ConfigurationService configurationService;

    @Mock 
    private AuditLogDifference auditLogDifference;

    private ClaimStatusMappings claimStatusMappings;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        claimStatusMappings = Struct.create(ClaimStatusMappings.class);
        claimStatusMappings.setCode("09");
        claimStatusMappings.setStatusCode("200");
        claimStatusMappings.setId("12");
        claimStatusMappings.setIsActiveEntity(true);
        claimStatusMappings.setIdentifier(10);
    }

    @Test
    public void beforeCreateClaimStatusTest() {
        handler.beforeCreateClaimStatus(claimStatusMappings);
    }

    // @Test
    // public void afterClaimStatusCreateUpdateTest() {
    //     List<ClaimStatusMappings> claimStatusMappingsList=new ArrayList<>();
    //     claimStatusMappingsList.add(claimStatusMappings);
    //     handler.afterClaimStatusCreateUpdate(claimStatusMappingsList);
    // }
    
    @Test
	public void onCreateClaimStatusMappingsTest() {
		when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
		handler.onCreateClaimStatus(claimStatusMappings);
	}
    
    @Test
	public void onCreateClaimStatusMappingsTestEmpty() {
    	claimStatusMappings.setIdentifier(null);
		when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
		handler.onCreateClaimStatus(claimStatusMappings);
	}
    
	@Test
	public void onCreateBusinessObjectConfigurationElseTest() {
		when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
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
		handler.onCreateClaimStatus(claimStatusMappings);

	}
    @Test
    public void afterClaimStatusDeletionTest() {
        Map<String, Object> targetKeys = new HashMap<>();
        targetKeys.put("ID", "ClaimStatus");
        when(cqnAnalyzerUtil.provideTargetKeys(Mockito.any())).thenReturn(targetKeys);
        //doNothing().when(auditLogHelper).logDeleteAuditData("1","ClaimStatus");
        handler.afterClaimStatusDeletion(cdsDeleteEventContext);
    }

    @Test
    public void onCreateBusinessObjectConfigurationElse1Test() {
        when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        // row.put("ID", "201");
        // row.put("identifier", "2");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateClaimStatus(claimStatusMappings);
    }


    @Test
    public void onCreateBusinessObjectConfigurationNullTest() {
        when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
         row.put("ID", "201");
         row.put("identifier", null);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateClaimStatus(claimStatusMappings);
    }


    @Test(expected = Exception.class)
    public void onCreateBusinessObjectConfigurationElse2Test() {
        when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
         row.put("ID", "201");
         row.put("identifier", "test");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateClaimStatus(claimStatusMappings);
    }

    @Test(expected = Exception.class)
    public void onCreateBusinessObjectConfigurationElse3Test() {
        when(claimStatusMappingsDao.getClaimStatusMappingsBasedOnIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(null);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateClaimStatus(claimStatusMappings);
    }

    @Test
    public void testLogUpsert() {
        
        ClaimStatusMappings mockedObj = ClaimStatusMappings.create();
        mockedObj.setId("test-id");
        when(configurationService.getclaimStatusMappingsDetails(anyString())).thenReturn(mockedObj);
        assertDoesNotThrow(() -> handler.logUpsert(mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        ClaimStatusMappings claimStatusMappings = ClaimStatusMappings.create();
        claimStatusMappings.setId("123");

        ClaimStatusMappings mockAction = mock(ClaimStatusMappings.class);
        when(configurationService.getclaimStatusMappingsDetails(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(claimStatusMappings));
    }

    @Test
    public void testLogDelete() {
        Mockito.when(configurationService.getclaimStatusMappingsDetails(anyString()))
                .thenReturn(ClaimStatusMappings.create());
        assertDoesNotThrow(() -> handler.logDelete("test-id"));
    }

    @Test
    public void testAfterClaimStatusCreateUpdate(){
                when(configurationService.getclaimStatusMappingsDetails(claimStatusMappings.getId())).thenReturn(claimStatusMappings);
         assertDoesNotThrow(
                () -> handler.afterClaimStatusCreateUpdate(claimStatusMappings));
    }

    @Test
    public void testBeforeClaimStatusUpdate(){
                when(configurationService.getclaimStatusMappingsDetails(claimStatusMappings.getId())).thenReturn(claimStatusMappings);
         assertDoesNotThrow(
                () -> handler.beforeClaimStatusUpdate(claimStatusMappings));
    }

    }
