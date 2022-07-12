package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintReasons;

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
import com.sap.ic.cmh.configuration.service.ComplaintReasonsService;
import com.sap.ic.cmh.configuration.validations.ComplaintReasonsValidation;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.gen.MessageKeys;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintReasonHandlerTest {
    @InjectMocks
    ComplaintReasonHandler handler;

    @Mock
    ComplaintReasonsValidation complaintReasonsValidation;

    @Mock
    ComplaintReasonsService complaintReasonsService;
    @Mock
    Messages messages;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    CdsReadEventContext context;

    @Mock
    Stream<ComplaintReasons> ComplaintReasonsStream;

    @Mock
    private AuditLogHelper<ComplaintReasons> auditLogHelper;

    private ComplaintReasons complaintItemReasons;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintItemReasons = Struct.create(ComplaintReasons.class);
        complaintItemReasons.setCode("reasoncode");
        complaintItemReasons.setIdentifier(10);
        complaintItemReasons.setDescription("yeah descp");
    }

    @Test
    public void beforeCreateComplainItemReasonTest() {
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
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_CODE)).thenReturn(message1);
        handler.beforeComplaintReasonCreation(complaintItemReasons);
    }

    @Test
    public void beforeUpdateComplainItemReasonTest() {
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
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_CODE)).thenReturn(message1);
        handler.beforeComplaintReasonUpdate(complaintItemReasons);
    }

    @Test
    public void onCreateComplaintItemReasonTest() {
        when(complaintReasonsService.getAllComplaintReasonsOrderByIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onComplaintReasonCreation(complaintItemReasons);
    }
    @Test
    public void onCreateComplaintItemReasonNullTest()
    {
        when(complaintReasonsService.getAllComplaintReasonsOrderByIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onComplaintReasonCreation(complaintItemReasons);

    }

    @Test
    public void onCreateComplaintItemReasonEmptyTest()
    {
        when(complaintReasonsService.getAllComplaintReasonsOrderByIdentifier()).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.empty();
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        handler.onComplaintReasonCreation(complaintItemReasons);
    }

    @Test
    public void afterComplaintReasonsReadNullTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }

    @Test
    public void afterComplaintReasonsReadTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(true);
        complaintItemReasons.setHasDraftEntity(true);
        complaintItemReasons.setIsActive(true);
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterComplaintReasonsReadNullIsActiveTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(true);
        complaintItemReasons.setHasDraftEntity(true);

        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterComplaintReasonsReadNullHasActiveTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(true);

        complaintItemReasons.setIsActive(true);
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterComplaintReasonsReadFalseTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(true);
        complaintItemReasons.setHasDraftEntity(false);
        complaintItemReasons.setIsActive(true);
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterComplaintReasonsReadFalseTest1()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(false);
        complaintItemReasons.setHasDraftEntity(true);
        complaintItemReasons.setIsActive(true);
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterComplaintReasonsReadTrueTest()
    {
        List<ComplaintReasons> list=new ArrayList<>();
        complaintItemReasons.setIsActiveEntity(true);
        complaintItemReasons.setHasDraftEntity(false);
        complaintItemReasons.setIsActive(false);
        list.add(complaintItemReasons);

        handler.afterComplaintReasonsRead(context,list.stream());
    }
    @Test
    public void afterUpdateComplaintReasonTest()
    {

        handler.afterUpdateComplaintReason(complaintItemReasons);
    }
    @Test
    public void afterComplaintReasonCreationTest()
    {

        handler.afterComplaintReasonCreation(complaintItemReasons);
    }
    @Test
    public void logUpsertTest()
    {
        handler.logUpsert(Action.UPDATE,complaintItemReasons);
    }
}