package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintChannels;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.auditlog.*;
import com.sap.cds.services.cds.CdsDeleteEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.persistency.ComplaintChannelDao;
import com.sap.ic.cmh.configuration.service.ComplaintChannelService;
import com.sap.ic.cmh.configuration.validations.ComplaintChannelValidationImpl;
import com.sap.ic.cmh.gen.MessageKeys;
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

public class ComplaintChannelConfigurationHandlerTest {
    @InjectMocks
    ComplaintChannelConfigurationHandler handler;

    @Mock
    ComplaintChannelValidationImpl complaintChannelValidation;

    @Mock
    ComplaintChannelService complaintChannelService;

    @Mock
    ComplaintChannelDao complaintChannelDao;

    @Mock
    Messages messages;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock
    Action action;

    private ComplaintChannels complaintChannels;

    private Row row;
    private Optional<Row> opt;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setCode("channelcode");
        complaintChannels.setIdentifier(77);
        complaintChannels.setDescription("this is descp");

        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("identifier", "11");
        opt = Optional.of(row);
    }

    @Test
    public void beforeCreateComplainChannelTest() {
        List<Row> rowValues = new ArrayList<>();
        when(messages.error(any(String.class),any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)).thenReturn(message1);
        handler.beforeComplaintChannelCreation(complaintChannels);
    }

    @Test
    public void beforeUpdateComplainChannelTest() {
        when(messages.error(any(String.class),any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error(MessageKeys.DUPLICATE_COMPLAINT_CHANNEL_CODE)).thenReturn(message1);
        handler.beforeComplaintChannelUpdate(complaintChannels);
    }

    @Test
    public void onCreateComplaintChannelTest() {
        when(complaintChannelService.getAllComplaintChannelsOrderByIdentifier()).thenReturn(result);
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", "2");
        row.put("code", "channelcode");
        Optional<Row> opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        handler.onComplaintChannelCreation(complaintChannels);
    }

    @Test
    public void testOnCreateComplaintChannelConfigurationTest() {
        when(complaintChannelService.getAllComplaintChannelsOrderByIdentifier()).thenReturn(result);
        handler.onComplaintChannelCreation(complaintChannels);
    }

    @Test
    public void onComplaintChannelCreationIdentifierNull() {
        when(complaintChannelService.getAllComplaintChannelsOrderByIdentifier()).thenReturn(result);
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("identifier", null);
        row.put("code", "channelcode");
        Optional<Row> opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        handler.onComplaintChannelCreation(complaintChannels);
    }

    @Test
    public void testfterReadComplaintChannelConfigurations(){
        List<ComplaintChannels> list=new ArrayList<>();
        list.add(complaintChannels);
        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintComplaintConfigurationsReadNullTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        list.add(complaintChannels);

        handler.afterComplaintChannel(list.stream());
    }

    @Test
    public void afterComplaintChannelConfigurationsReadTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setHasDraftEntity(true);
        complaintChannels.setIsActive(true);
        list.add(complaintChannels);

        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintChannelConfigurationsReadNullIsActiveTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setHasDraftEntity(true);

        list.add(complaintChannels);

        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintChannelConfigurationsReadNullHasActiveTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setIsActive(true);
        list.add(complaintChannels);
        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintChannelConfigurationsReadFalseTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setHasDraftEntity(false);
        complaintChannels.setIsActive(true);
        list.add(complaintChannels);

        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintChannelConfigurationsReadFalseTest1()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(false);
        complaintChannels.setHasDraftEntity(true);
        complaintChannels.setIsActive(true);
        list.add(complaintChannels);
        handler.afterComplaintChannel(list.stream());
    }
    @Test
    public void afterComplaintcChannelConfigurationsReadTrueTest()
    {
        List<ComplaintChannels> list=new ArrayList<>();
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setHasDraftEntity(false);
        complaintChannels.setIsActive(false);
        list.add(complaintChannels);
        handler.afterComplaintChannel(list.stream());
    }

    @Test
    public void onUpdateComplaintChannelTest(){
        handler.afterUpdateComplaintChannel(complaintChannels);
    }

    @Test
    public void afterUpdateComplaintChannelTest(){
        handler.afterUpdateComplaintChannel(complaintChannels);
    }

    @Test
    public void afterCreateComplaintChannelTest(){
        handler.afterCreateComplaintChannel(complaintChannels);
    }

    @Test
    public void logUpsertTest(){
        handler.logUpsert(action,complaintChannels);
    }

}
