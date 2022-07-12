package com.sap.ic.cmh.configuration.action.handler;
import cds.gen.configurationservice.ComplaintReasonMappings;
import cds.gen.configurationservice.DeactivateComplaintReasonMappingsContext;
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

public class DeactivateComplaintReasonMappingsHandlerTest {
    @InjectMocks
    DeactivateComplaintReasonMappingsHandler handler;

    @Mock
    DeactivateComplaintReasonMappingsContext context;

    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    private
    ComplaintReasonMappings complaintReasonMaps;
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
        complaintReasonMaps=Struct.create(ComplaintReasonMappings.class);

    }

    @Test
    public void deactivateComplaintReasonsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonMaps);
        complaintReasonMaps.setIsActiveEntity(true);
        complaintReasonMaps.setIsActive(false);
        handler.deactivateComplaintReasonMappings(context);
    }
    @Test
    public void deactivateComplaintReasonsIsActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonMaps);
        complaintReasonMaps.setIsActiveEntity(true);
        complaintReasonMaps.setIsActive(true);
        handler.deactivateComplaintReasonMappings(context);
    }
    @Test
    public void deactivateComplaintReasonsIsInActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonMaps);
        complaintReasonMaps.setIsActiveEntity(false);
        handler.deactivateComplaintReasonMappings(context);
    }
    @Test
    public void afterDeactivateComplaintReasonMappingTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintReasonMappings.class)).thenReturn(complaintReasonMaps);
        handler.afterDeactivateComplaintReasonMapping(context);
    }

}
