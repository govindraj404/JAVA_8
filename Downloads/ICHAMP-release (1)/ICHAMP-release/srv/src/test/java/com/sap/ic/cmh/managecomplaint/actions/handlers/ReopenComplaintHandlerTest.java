package com.sap.ic.cmh.managecomplaint.actions.handlers;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.ReopenContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
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

public class ReopenComplaintHandlerTest {
    @InjectMocks
    @Autowired
    ReopenComplaintHandler reopenComplaintHandler;
    @Mock
    Messages messages;
    @Mock
    ComplaintsDao complaintsDao;
    @Mock
    ReopenContext context;
    @Mock
    CdsService service;
    @Mock
    Result result;

    Complaints complaints;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        complaints = Struct.create(Complaints.class);
        when(context.getService()).thenReturn(service);
        when(service.run(ArgumentMatchers.<CqnSelect>any())).thenReturn(result);
        when(result.single(Complaints.class)).thenReturn(complaints);
    }

    @Test
    public void testSuccessfulReopen() {
        complaints.setIsActiveEntity(true);
        complaints.setComplaintStatusCode(Constants.COMPLAINT_CLOSED);
        reopenComplaintHandler.reopen(context);

        verify(messages).success(MessageKeys.COMPLAINT_REOPEN_ACTION_SUCCESSFULLY_PERFORMED);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }

    @Test
    public void testUnsuccessfulReopen() {
        complaints.setIsActiveEntity(true);
        complaints.setComplaintStatusCode(Constants.COMPLAINT_IN_PROGRESS);
        reopenComplaintHandler.reopen(context);

        verify(messages).success(MessageKeys.COMPLAINT_REOPEN_ACTION_NOT_SUCCESSFULLY_PERFORMED);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }

    @Test
    public void testUnsuccessfulReopenOnDraftRecord() {
        complaints.setIsActiveEntity(false);
        reopenComplaintHandler.reopen(context);

        verify(messages).success(MessageKeys.COMPLAINT_REOPEN_ACTION_NOT_SUCCESSFULLY_PERFORMED_FOR_DRAFT_RECORD);
        verify(context).setResult(complaints);
        verify(context).setCompleted();
    }
}
