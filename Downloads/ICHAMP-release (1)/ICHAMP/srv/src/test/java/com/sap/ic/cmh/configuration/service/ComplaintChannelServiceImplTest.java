package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ComplaintChannels;
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
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ComplaintChannelServiceImplTest {
    @InjectMocks
    ComplaintChannelServiceImpl complaintChannelService;

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
    public void beforeCreateComplaintChannelTest() {
        when(complaintChannelDao.getAllComplaintChannelsCodeAndIDByCode(any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "channelcode");
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
        complaintChannelService.getAllComplaintChannelsOrderByIdentifier();
    }

    @Test
    public void testGetComplaintChannelDetails() {
        when(complaintChannelDao.getComplaintChannelDetailsBasedOnId(any(String.class))).thenReturn(result);
        complaintChannelService.getComplaintChannelDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

    @Test
    public void testGetComplaintChannelDetailsCheckFalseCondition() {
        when(complaintChannelDao.getComplaintChannelDetailsBasedOnId(any(String.class))).thenReturn(result);
        List<ComplaintChannels> complaintChannelsList = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        Optional<Row> opt = Optional.of(row);
        complaintChannelsList.add(complaintChannels);
        when(result.first()).thenReturn(opt);
        when(result.listOf(ComplaintChannels.class)).thenReturn(complaintChannelsList);
        complaintChannelService.getComplaintChannelDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

}
