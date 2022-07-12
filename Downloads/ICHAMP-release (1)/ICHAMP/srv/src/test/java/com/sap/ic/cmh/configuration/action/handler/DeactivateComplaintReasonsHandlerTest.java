package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintReasons;
import cds.gen.configurationservice.DeactivateComplaintReasonsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ComplaintReasonHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class DeactivateComplaintReasonsHandlerTest {

    @InjectMocks
    DeactivateComplaintReasonsHandler handler;

    @Mock
    DeactivateComplaintReasonsContext context;

    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    private
    ComplaintReasons complaintReasons;
    @Mock
    Result result;

    @Mock
    Messages messages;
    @Mock
    protected PersistenceService db;

    @Mock
    AuditLogHelper<ComplaintReasons> auditLogHelper;

    @Mock
    private ComplaintReasonHandler crHandler;

    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);
        complaintReasons=Struct.create(ComplaintReasons.class);

    }

    @Test
    public void deactivateComplaintReasonsTest()
    {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasons.class)).thenReturn(complaintReasons);
        complaintReasons.setIsActiveEntity(true);
        complaintReasons.setIsActive(false);
        handler.onDeactivateComplaintReasons(context);
    }
    @Test
    public void deactivateComplaintReasonsIsActiveTest()
    {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasons.class)).thenReturn(complaintReasons);
        complaintReasons.setIsActiveEntity(true);
        complaintReasons.setIsActive(true);
        handler.onDeactivateComplaintReasons(context);
    }
    @Test
    public void deactivateComplaintReasonsIsInActiveTest()
    {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasons.class)).thenReturn(complaintReasons);
        complaintReasons.setIsActiveEntity(false);
        handler.onDeactivateComplaintReasons(context);
    }

    @Test
    public void afterDeactivateComplaintReasonsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasons.class)).thenReturn(complaintReasons);
        handler.afterDeactivateComplaintReasons(context);
    }

}