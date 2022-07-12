package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.DestinationConfigurations;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintChannelDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ComplaintChannelValidationImplTest {
    @InjectMocks
    ComplaintChannelValidationImpl complaintChannelValidation;

    @Mock
    ComplaintChannelDao complaintChannelDao;

    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;
    @Mock
    Result result;

    ComplaintChannels complaintChannels;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    Message message1;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setCode("channelcode");
        complaintChannels.setIdentifier(77);
        complaintChannels.setDescription("this is descp");
    }

    @Test
    public void checkInputForUnwantedChars() {
        complaintChannels.setCode("<html><head>");
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        complaintChannelValidation.checkInputsSanitized(complaintChannels);
    }

    @Test
    public void checkInputForNull() {
        complaintChannels.setCode(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.COMPLAINT_CHANNEL_CODE_IS_MANDATORY)).thenReturn(msg);
        complaintChannelValidation.checkInputsSanitized(complaintChannels);
    }

    @Test
    public void checkInputForCodeBlank() {
        complaintChannels.setCode("");
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(null);
        when(msg.target(any(String.class))).thenReturn(null);
        when(messages.error(MessageKeys.COMPLAINT_CHANNEL_CODE_IS_MANDATORY)).thenReturn(msg);
        complaintChannelValidation.checkInputsSanitized(complaintChannels);
    }

    @Test
    public void checkValidationComplaintChannelCodeExistsEmptyList() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("code", "channelcode");
        Optional<Row> opt = Optional.of(row);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(any(String.class))).thenReturn(result);
        complaintChannelValidation.validationComplaintChannelCodeExists(complaintChannels);
    }

    @Test
    public void checkValidationComplaintChannelCodeExistsSameCode() {
        ComplaintChannels complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setId("201");
        complaintChannels.setCode("channelcode");
        complaintChannels.setIdentifier(77);
        complaintChannels.setDescription("this is descp");
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "channelcode");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(any(String.class))).thenReturn(result);
        complaintChannelValidation.validationComplaintChannelCodeExists(complaintChannels);
    }

    @Test
    public void beforeExistCreateComplainChannelTestNULLcode() {
        MockitoAnnotations.openMocks(this);
        when(complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(any(String.class))).thenReturn(null);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "Hi");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setCode("Hello");
        complaintChannels.setIdentifier(10);
        complaintChannels.setId("1021");
        complaintChannels.setDescription("this is descp");
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)).thenReturn(message1);
        when(complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(anyString())).thenReturn(result);
        complaintChannelValidation.validateComplaintChannel(complaintChannels);
    }
}
