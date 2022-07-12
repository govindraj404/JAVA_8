package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.TargetTypes;
import cds.gen.configurationservice.TargetTypes_;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.configuration.validations.TargetTypeConfigurationValidationImpl;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.cds.services.auditlog.Action;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class TargetTypeConfigurationHandlerTest {
    @InjectMocks
    TargetTypeConfigurationHandler handler;

    @Mock
    TargetTypeConfigurationValidationImpl targetTypeConfigurationValidation;

    @Mock
    TargetTypeConfigurationService targetTypeConfigurationService;

    @Mock
    Messages messages;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Action action;
    private TargetTypes targetTypes;
    @Mock
    private AuditLogHelper<TargetTypes> auditLogHelper;
    private Row row;
    private Optional<Row> opt;

    List<TargetTypes> targetTypesList=new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        targetTypes = Struct.create(TargetTypes.class);
        targetTypes.setCode("channelcode");
        targetTypes.setIdentifier(77);
        targetTypes.setDescription("this is descp");
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        opt = Optional.of(row);

    }

    @Test
    public void beforeCreateTargetTypeTest() {
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class),any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)).thenReturn(message1);
        handler.beforeTargetTypeCreateUpdate(targetTypes);
    }
    @Test
    public void beforeUpdateTargetTypeTest() {
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class),any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)).thenReturn(message1);
        handler.beforeTargetTypeUpdate(targetTypes);
    }

    @Test
    public void onCreateComplaintChannelTest() {
        when(targetTypeConfigurationService.getTargetTypeConfigurations()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "channelcode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateTargetType(targetTypes);
    }

    @Test
    public void onCreateTargetTypeFalseConditionTest() {
        when(targetTypeConfigurationService.getTargetTypeConfigurations()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        row.put("code", "Targettype");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onCreateTargetType(targetTypes);
    }

    @Test
    public void testOnCreateTargetTypeConfigurationTest() {
        when(targetTypeConfigurationService.getTargetTypeConfigurations()).thenReturn(result);
        handler.onCreateTargetType(targetTypes);
    }

    @Test
    public void afterTargetTypeReadNullTest(){
        List<TargetTypes> list=new ArrayList<>();
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }

    @Test
    public void afterTargetTypeReadTest(){
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(true);
        targetTypes.setHasDraftEntity(true);
        targetTypes.setIsActive(true);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void afterTargetTypeReadNullIsActiveTest(){
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(true);
        targetTypes.setHasDraftEntity(true);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void afterTargetTypeReadNullHasActiveTest()  {
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(true);
        targetTypes.setIsActive(true);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void afterTargetTypeReadFalseTest(){
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(true);
        targetTypes.setHasDraftEntity(false);
        targetTypes.setIsActive(true);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void afterTargetTypeReadFalseTest1() {
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(false);
        targetTypes.setHasDraftEntity(true);
        targetTypes.setIsActive(true);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void afterTargetTypeReadTrueTest() {
        List<TargetTypes> list=new ArrayList<>();
        targetTypes.setIsActiveEntity(true);
        targetTypes.setHasDraftEntity(false);
        targetTypes.setIsActive(false);
        list.add(targetTypes);
        handler.afterTargetType(list.stream());
    }
    @Test
    public void testAfterUpdateTargetType() {
        handler.afterUpdateTargetType(targetTypes);
    }
    @Test
    public void testAfterCreateTargetType() {
        handler.afterCreateTargetType(targetTypes);
    }
    @Test
    public void logUpsertTest(){
        handler.logUpsert(action,targetTypes);
    }

}
