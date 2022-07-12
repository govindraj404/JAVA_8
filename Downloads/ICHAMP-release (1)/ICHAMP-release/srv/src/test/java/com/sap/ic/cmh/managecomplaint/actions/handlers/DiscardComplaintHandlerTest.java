package com.sap.ic.cmh.managecomplaint.actions.handlers;

import cds.gen.complaintservice.DiscardContext;
import cds.gen.complaintservice.Complaints;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import java.util.ArrayList;

public class DiscardComplaintHandlerTest {
    @InjectMocks
    @Autowired
    DiscardComplaintHandler discardComplaintHandler;
    @Mock
    Messages messages;
    @Mock
    ComplaintsDao complaintsDao;
    @Mock
    StreamService streamService;
    @Mock
    DiscardContext context;
    @Mock
    CdsService service;
    @Mock
    Result result;
    @Mock
	BusinessObjectDao businessObjectDao;

    Complaints complaints;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        complaints = Struct.create(Complaints.class);
        complaints.setId("123");
        when(context.getService()).thenReturn(service);
        when(service.run(ArgumentMatchers.<CqnSelect>any())).thenReturn(result);
        when(result.single(Complaints.class)).thenReturn(complaints);
    }

    @Test
    public void testDiscard() {
        complaints.setIsActiveEntity(true);
        complaints.setComplaintStatusCode(Constants.COMPLAINT_CREATED_STATUS);
        discardComplaintHandler.discard(context);
        verify(messages).success(MessageKeys.COMPLAINT_DISCARD_ACTION_SUCCESSFULLY_PERFORMED);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }

    @Test
    public void testDiscardOnActiveBO() {
        complaints.setIsActiveEntity(true);
        complaints.setComplaintStatusCode(Constants.COMPLAINT_IN_PROGRESS);
        discardComplaintHandler.discard(context);
        verify(messages).success(MessageKeys.COMPLAINT_DISCARD_ACTION_NOT_SUCCESSFULLY_PERFORMED_WHEN_BOS_ARE_ACTIVE);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }

    @Test
    public void testDiscardOnDraftRecord() {
        complaints.setIsActiveEntity(false);
        discardComplaintHandler.discard(context);
        verify(messages).success(MessageKeys.COMPLAINT_DISCARD_ACTION_NOT_SUCCESSFULLY_PERFORMED_FOR_DRAFT_RECORD);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }
}
