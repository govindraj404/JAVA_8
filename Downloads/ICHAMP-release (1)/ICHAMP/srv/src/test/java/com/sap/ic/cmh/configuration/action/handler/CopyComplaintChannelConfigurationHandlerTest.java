package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintChannels;
import cds.gen.configurationservice.CopyComplaintChannelsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CopyComplaintChannelConfigurationHandlerTest {
    @Mock
    DraftService draftService;
    @InjectMocks
    CopyComplaintChannelConfigurationHandler handler;
    @Mock
    MessageKeys keys;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    CqnInsert cqnInsert;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Messages messages;
    @Mock
    Message msg;

    private ComplaintChannels complaintChannels;
    @Mock
    private CopyComplaintChannelsContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintChannels = Struct.create(ComplaintChannels.class);
        complaintChannels.setCode("xyz");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void copyComplaintChannelTest() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintChannels.class)).thenReturn(complaintChannels);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        when(messages1.success(any(String.class),any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        handler.copyComplaintChannels(context);
    }
}

