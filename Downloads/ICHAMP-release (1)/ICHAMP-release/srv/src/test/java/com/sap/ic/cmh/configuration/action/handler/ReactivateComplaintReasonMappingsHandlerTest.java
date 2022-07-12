package com.sap.ic.cmh.configuration.action.handler;
import cds.gen.configurationservice.ComplaintReasonMappings;
import cds.gen.configurationservice.ReactivateComplaintReasonMappingsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ComplaintReasonMappingsHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ReactivateComplaintReasonMappingsHandlerTest {

    @InjectMocks
    ReactivateComplaintReasonMappingsHandler handler;
    @Mock
    ReactivateComplaintReasonMappingsContext context;
    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    private
    ComplaintReasonMappings complaintReasonsMap;
    @Mock
    Result result;

    @Mock
    Messages messages;
    @Mock
    protected PersistenceService db;

    @Mock
    AuditLogHelper<ComplaintReasonMappings> auditLogHelper;

    @Mock
    ComplaintReasonMappingsHandler complaintReasonMappingsHandler;

    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);
        complaintReasonsMap= Struct.create(ComplaintReasonMappings.class);
    }

    @Test
    public void reactivateComplaintReasonsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonsMap);
        complaintReasonsMap.setIsActiveEntity(true);
        complaintReasonsMap.setIsActive(false);
        handler.reactivateComplaintReasonMappings(context);
    }
    @Test
    public void reactivateComplaintReasonsIsActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonsMap);
        complaintReasonsMap.setIsActiveEntity(true);
        complaintReasonsMap.setIsActive(true);
        handler.reactivateComplaintReasonMappings(context);
    }
    @Test
    public void reactivateComplaintReasonsIsInActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonsMap);
        complaintReasonsMap.setIsActiveEntity(false);
        handler.reactivateComplaintReasonMappings(context);
    }


    @Test
    public void afterReactivateComplaintReasonMappingTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonsMap);
        handler.afterReactivateComplaintReasonMapping(context);
    }

}
