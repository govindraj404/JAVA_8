package com.sap.ic.cmh.configuration.handler;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.ReactivateComplaintChannelsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ReactivateComplaintChannelHandlerTest {

    @InjectMocks
    ReactiveComplaintChannelHandler handler;

    @Mock
    ComplaintChannels complaintChannels;

    @Mock
    Messages messages;

    @Mock
    PersistenceService db;

    @Mock
    ReactivateComplaintChannelsContext context;

    @Mock
    Result result;

    @Mock
    Message msg;

    @Mock
    private CdsService cdsService;

    @Mock
    private CqnSelect cqnSelect;

    @Mock
    AuditLogHelper<ComplaintChannels> auditLogHelper;

    @Mock
    private ComplaintChannelConfigurationHandler complaintChannelConfigurationHandler;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setId("143");
        complaintChannels.setCode("code");
    }

    @Test
    public void reactiveComplaintChannelTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setIsActive(true);
        when(result.single(ComplaintChannels.class)).thenReturn(complaintChannels);
        handler.reactivateComplaintChannels(context);
    }

    @Test
    public void reactiveComplaintChannelTestElse() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannels.setIsActiveEntity(true);
        complaintChannels.setIsActive(false);
        when(result.single(ComplaintChannels.class)).thenReturn(complaintChannels);
        handler.reactivateComplaintChannels(context);
    }

    @Test
    public void reactiveComplaintChannelTestElse1() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintChannels.setIsActiveEntity(false);
        when(result.single(ComplaintChannels.class)).thenReturn(complaintChannels);
        handler.reactivateComplaintChannels(context);
    }

    @Test
    public void afterDeactivateComplaintChannelsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintChannels.class)).thenReturn(complaintChannels);
        handler.afterReactivateComplaintChannels(context);
    }
}