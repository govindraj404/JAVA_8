package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintReasons;
import cds.gen.configurationservice.CopyComplaintReasonsContext;
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

public class CopyComplaintReasonsHandlerTest {

    @Mock
    DraftService draftService;
    @InjectMocks
    CopyComplaintReasonsHandler handler;
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

    private ComplaintReasons complaintItemReasons;
    @Mock
    private CopyComplaintReasonsContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintItemReasons = Struct.create(ComplaintReasons.class);
        complaintItemReasons.setCode("abc");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void copyComplaintItemReasonTest() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasons.class)).thenReturn(complaintItemReasons);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        //when(messages.success(any(String.class))).thenReturn(msg);
        when(messages1.success(any(String.class),any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        //when(messages.success(MessageKeys.COMPLAINT_ITEM_REASON_OBJECT_COPIED),any(Object[].class)).thenReturn(msg);
        handler.copyComplaintReason(context);
    }
}
