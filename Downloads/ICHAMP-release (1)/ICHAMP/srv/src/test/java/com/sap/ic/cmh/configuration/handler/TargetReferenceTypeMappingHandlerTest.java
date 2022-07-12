package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintReasons;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.service.targetreferencetypemappings.TargetReferenceTypeMappingService;
import com.sap.ic.cmh.configuration.validations.TargetReferenceTypesValidation;
import com.sap.ic.cmh.configuration.validations.targetreferencetypemappings.TargetReferenceTypeMappingValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class TargetReferenceTypeMappingHandlerTest {

    @InjectMocks
    TargetReferenceTypeMappingHandler handler;
    @Mock
    Messages messages;
    @Mock
    Message message1;

    @Mock
    CdsReadEventContext context;

    @Mock
    TargetReferenceTypeMappingValidation targetReferenceTypeMappingValidator;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;

    @Mock
    Optional<Row> targetReferenceTypeMappingFirst;
    @Mock
    TargetReferenceTypesValidation targetReferenceTypesValidator;
    @Mock
    TargetReferenceTypeMappings targetReferenceTypeMapping;

    @Mock
    TargetReferenceTypeMappingService targetReferenceTypeMappingService;
    @Mock
    private AuditLogHelper<ComplaintReasons> auditLogHelper;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        targetReferenceTypeMapping = Struct.create(TargetReferenceTypeMappings.class);

    }

    @Test
    public void afterUpdateTargetReferenceTypeMappingTest() {
        handler.afterUpdateTargetReferenceTypeMapping(targetReferenceTypeMapping);
    }
    @Test
    public void afterCreateTargetReferenceTypeMappingTest() {
        handler.afterCreateTargetReferenceTypeMapping(targetReferenceTypeMapping);
    }
    @Test
    public void beforeTargetReferenceTypeMappingCreateUpdateTest() {
        handler.beforeTargetReferenceTypeMappingCreateUpdate(targetReferenceTypeMapping);
    }

    @Test
    public void onCreateTargetReferenceTypeMappingTest() {

        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappings()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "1");
        targetReferenceTypeMappingFirst = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(targetReferenceTypeMappingFirst);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateTargetReferenceTypeMapping(targetReferenceTypeMapping);

    }

    @Test
    public void onCreateTargetReferenceTypeMappingNullTest() {
        when(targetReferenceTypeMappingService.getTargetReferenceTypeMappings()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        targetReferenceTypeMappingFirst = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(targetReferenceTypeMappingFirst);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateTargetReferenceTypeMapping(targetReferenceTypeMapping);

    }


    @Test
    public void afterTargetReferenceTypeMappingReadNullTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }

    @Test
    public void afterTargetReferenceTypeMappingReadReadTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(true);
        targetReferenceTypeMapping.setHasDraftEntity(true);
        targetReferenceTypeMapping.setIsActive(true);
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }
    @Test
    public void afterTargetReferenceTypeMappingReadNullIsActiveTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(true);
        targetReferenceTypeMapping.setHasDraftEntity(true);

        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }
    @Test
    public void afterTargetReferenceTypeMappingReadNullHasActiveTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(true);

        targetReferenceTypeMapping.setIsActive(true);
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }
    @Test
    public void afterTargetReferenceTypeMappingReadFalseTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(true);
        targetReferenceTypeMapping.setHasDraftEntity(false);
        targetReferenceTypeMapping.setIsActive(true);
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }
    @Test
    public void afterTargetReferenceTypeMappingReadFalseTest1()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(false);
        targetReferenceTypeMapping.setHasDraftEntity(true);
        targetReferenceTypeMapping.setIsActive(true);
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }
    @Test
    public void afterTargetReferenceTypeMappingReadTrueTest()
    {
        List<TargetReferenceTypeMappings> list=new ArrayList<>();
        targetReferenceTypeMapping.setIsActiveEntity(true);
        targetReferenceTypeMapping.setHasDraftEntity(false);
        targetReferenceTypeMapping.setIsActive(false);
        list.add(targetReferenceTypeMapping);

        handler.afterTargetReferenceTypeMappingRead(context,list.stream());
    }

    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,targetReferenceTypeMapping);
    }
    @Test
    public void beforeTargetReferenceTypeMappingUpdateTest()
    {
        handler.beforeTargetReferenceTypeMappingUpdate(targetReferenceTypeMapping);
    }
}