package com.sap.ic.cmh.configuration.handler;


import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.ComplaintTypeToItemCatMappingService;
import com.sap.ic.cmh.configuration.validations.ComplaintTypeToItemCatMappingValidation;
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

public class ComplaintTypeToItemCatMappingHandlerTest {

    @InjectMocks
    ComplaintTypeToItemCatMappingHandler handler;

    @Mock
    ComplaintTypeToItemCatMappingValidation complaintTypeToItemCatMappingValidation;

    @Mock
    ComplaintTypeToItemCatMappingService complaintTypeToItemCatMappingService;

    @Mock
    Messages messages;

    @Mock
    Message message;
    @Mock
    Result result;

    @Mock
    protected PersistenceService mockDb;

    private ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings;

    private Row row;
    private Optional<Row> opt;
    @Mock
    CdsReadEventContext context;
    @Mock
    private AuditLogHelper<ComplaintTypeToItemCategoryMappings> auditLogHelper;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintTypeToItemCategoryMappings = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintTypeToItemCategoryMappings.setComplaintTypeId("1234");
        complaintTypeToItemCategoryMappings.setItemCategoryId("1245");
        complaintTypeToItemCategoryMappings.setId("1245");
        complaintTypeToItemCategoryMappings.setDivisionId("127");
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        opt = Optional.of(row);
    }

    @Test
    public void beforeComplaintTypeToItemCategoryMappingCreateTest() {
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        when(message.target(any(String.class))).thenReturn(message);
        when(messages.error("abc")).thenReturn(message);
        handler.beforeComplaintTypeToItemCategoryMappingCreate(complaintTypeToItemCategoryMappings);
    }

    @Test
    public void onComplaintTypeToItemCategoryMappingCreateTest() {
        when(complaintTypeToItemCatMappingService.getComplaintTypeToItemCatMapping()).thenReturn(result);
        when(result.first()).thenReturn(opt);
        handler.onComplaintTypeToItemCategoryMappingCreate(complaintTypeToItemCategoryMappings);
    }

    @Test
    public void onComplaintTypeToItemCategoryMappingCreateEmptyTest() {
        Optional<Row> emptyOpt = Optional.empty();
        when(complaintTypeToItemCatMappingService.getComplaintTypeToItemCatMapping()).thenReturn(result);
        when(result.first()).thenReturn(emptyOpt);
        handler.onComplaintTypeToItemCategoryMappingCreate(complaintTypeToItemCategoryMappings);
    }

    @Test
    public void onComplaintTypeToItemCategoryMappingCreateElseTest() {
        when(complaintTypeToItemCatMappingService.getComplaintTypeToItemCatMapping()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        handler.onComplaintTypeToItemCategoryMappingCreate(complaintTypeToItemCategoryMappings);
    }

    @Test
    public void beforeComplaintTypeToItemCategoryMappingUpdateTest() {

        handler.beforeComplaintTypeToItemCategoryMappingUpdate(complaintTypeToItemCategoryMappings);
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadNullTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }

    @Test
    public void afterComplaintTypeToItemCatMapReadTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(true);
        complaintTypeToItemCategoryMappings.setHasDraftEntity(true);
        complaintTypeToItemCategoryMappings.setIsActive(true);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadNullIsActiveTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(true);
        complaintTypeToItemCategoryMappings.setHasDraftEntity(true);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadNullHasActiveTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(true);
        complaintTypeToItemCategoryMappings.setIsActive(true);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadFalseTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(true);
        complaintTypeToItemCategoryMappings.setHasDraftEntity(false);
        complaintTypeToItemCategoryMappings.setIsActive(true);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadFalseTest1()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(false);
        complaintTypeToItemCategoryMappings.setHasDraftEntity(true);
        complaintTypeToItemCategoryMappings.setIsActive(true);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterComplaintTypeToItemCatMapReadTrueTest()
    {
        List<ComplaintTypeToItemCategoryMappings> list=new ArrayList<>();
        complaintTypeToItemCategoryMappings.setIsActiveEntity(true);
        complaintTypeToItemCategoryMappings.setHasDraftEntity(false);
        complaintTypeToItemCategoryMappings.setIsActive(false);
        list.add(complaintTypeToItemCategoryMappings);
        handler.afterComplaintTypeToItemCatMapRead(context,list.stream());
    }
    @Test
    public void afterUpdateCompTypeToItemCatMapTest()
    {
        handler.afterUpdateCompTypeToItemCatMap(complaintTypeToItemCategoryMappings);
    }
    @Test
    public void afterCreateCompTypeToItemCatMapTest()
    {
        handler.afterCreateCompTypeToItemCatMap(complaintTypeToItemCategoryMappings);
    }
    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,complaintTypeToItemCategoryMappings);
    }
}
