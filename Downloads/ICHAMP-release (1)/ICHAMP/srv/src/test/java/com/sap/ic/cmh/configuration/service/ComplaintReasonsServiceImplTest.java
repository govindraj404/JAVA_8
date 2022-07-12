package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintReasons;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.gen.MessageKeys;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ComplaintReasonsServiceImplTest {
    @InjectMocks
    ComplaintReasonsServiceImpl complaintReasonsService;
    @Mock
    ComplaintReasonsDao complaintReasonsDao;
    @Mock
    Messages messages;
    @Mock
    Message message1;
    @Mock
    Result result;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    public Row row;
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
    public void beforeCreateComplainItemReasonIDTest() {
        when(complaintReasonsDao.getComplaintReasonCodeAndIdByCode(any(String.class))).thenReturn(result);
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
        complaintReasonsService.getAllComplaintReasonsOrderByIdentifier();
    }
    @Test
    public void getComplaintReasonBasedOnIDTest()
    {
        when(complaintReasonsDao.getComplaintReasonDetailsBasedOnID(any(String.class))).thenReturn(result);
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
        List<ComplaintReasons> list=new ArrayList<>();
        list.add(complaintItemReasons);
        when(result.listOf(ComplaintReasons.class)).thenReturn(list);
        complaintReasonsService.getComplaintReasonBasedOnID("1");
    }
    @Test
    public void getComplaintReasonBasedOnIDEmptyTest()
    {
        when(complaintReasonsDao.getComplaintReasonDetailsBasedOnID(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        List<ComplaintReasons> list=new ArrayList<>();
        when(result.listOf(ComplaintReasons.class)).thenReturn(list);
        complaintReasonsService.getComplaintReasonBasedOnID("1");
    }
    @Test
    public void getActiveTest() {
        when(complaintReasonsDao.getComplaintReasonDetailsBasedOnID(anyString())).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ComplaintReasons complaintReasons = Struct.create(ComplaintReasons.class);
        complaintReasons.setId("ID");
        complaintReasons.setIsActive(true);
        List<ComplaintReasons> complaintReasons1 = new ArrayList<>();
        complaintReasons1.add(complaintReasons);
        when(result.listOf(ComplaintReasons.class)).thenReturn(complaintReasons1);
        complaintReasonsService.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
    @Test
    public void getActivenullTest() {
        when(complaintReasonsDao.getComplaintReasonDetailsBasedOnID(anyString())).thenReturn(result);
        complaintReasonsService.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
}