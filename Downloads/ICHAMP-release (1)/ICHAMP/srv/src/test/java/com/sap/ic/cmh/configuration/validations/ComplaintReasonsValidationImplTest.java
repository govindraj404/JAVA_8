package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintReasons;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.gen.MessageKeys;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ComplaintReasonsValidationImplTest {
    @InjectMocks
    ComplaintReasonsValidationImpl complaintItemReasonsValidation;

    @Mock
    ComplaintReasonsDao complaintReasonsDao;

    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;
    @Mock
    Result result;

    ComplaintReasons complaintItemReasons;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    Message message1;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintItemReasons = Struct.create(ComplaintReasons.class);
        complaintItemReasons.setCode("reasoncode");
        complaintItemReasons.setIdentifier(10);
        complaintItemReasons.setDescription("yeah descp");
    }

    @Test
    public void checkInputForUnwantedChars() {
        complaintItemReasons.setCode("<html><head>");
        Mockito.when(messages.error("any", "test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        complaintItemReasonsValidation.checkInputsSanitized(complaintItemReasons);
    }

    @Test
    public void checkInputForNull() {
        complaintItemReasons.setCode(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.COMPLAINT_REASON_CODE_IS_MANDATORY)).thenReturn(msg);
        complaintItemReasonsValidation.checkInputsSanitized(complaintItemReasons);
    }

    @Test
    public void checkInputForBlank() {
        complaintItemReasons.setCode("");
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.COMPLAINT_REASON_CODE_IS_MANDATORY)).thenReturn(msg);
        complaintItemReasonsValidation.checkInputsSanitized(complaintItemReasons);
    }
    @Test
    public void beforeCreateComplainItemReasonTest() {
        when(complaintReasonsDao.getComplaintReasonCodeAndIdByCode(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
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
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_CODE)).thenReturn(message1);
        complaintItemReasonsValidation.validationOnComplaintReasonCodeExists(complaintItemReasons);
    }

    @Test
    public void beforeExistCreateComplainItemReasonIDTest() {
        MockitoAnnotations.openMocks(this);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
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
        complaintItemReasons = Struct.create(ComplaintReasons.class);
        complaintItemReasons.setCode("reasoncode");
        complaintItemReasons.setIdentifier(10);
        complaintItemReasons.setId("1021");
        complaintItemReasons.setDescription("yeah descp");
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_REASON_CODE)).thenReturn(message1);
        when(complaintReasonsDao.getComplaintReasonCodeAndIdByCode(anyString())).thenReturn(result);
        complaintItemReasonsValidation.validateComplaintReason(complaintItemReasons);
    }

    @Test
    public void beforeCreateComplainItemReasonEmptyListTest() {
        when(complaintReasonsDao.getComplaintReasonCodeAndIdByCode(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        complaintItemReasonsValidation.validationOnComplaintReasonCodeExists(complaintItemReasons);
    }

    @Test
    public void beforeCreateComplainItemReasonNullTest() {
        complaintItemReasons.setId("101");
        when(complaintReasonsDao.getComplaintReasonCodeAndIdByCode(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "101");
        row.put("identifier", "2");
        row.put("code", "reasoncode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        complaintItemReasonsValidation.validationOnComplaintReasonCodeExists(complaintItemReasons);
    }



}
