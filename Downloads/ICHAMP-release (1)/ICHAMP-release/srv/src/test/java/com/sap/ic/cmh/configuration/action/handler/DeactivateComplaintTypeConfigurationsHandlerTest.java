package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.DeactivateComplaintTypeConfigurationsContext;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ComplaintTypeConfigurationHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class DeactivateComplaintTypeConfigurationsHandlerTest {

    @InjectMocks
    DeactivateComplaintTypeConfigurationsHandler handler;
    @Mock
    ComplaintTypeConfigurations complaintType;

    @Mock
    EventHandler eventHandler;

    @Mock
    Messages messages;

    @Mock
    PersistenceService db;

    @Mock
    DeactivateComplaintTypeConfigurationsContext context;

    @Mock
    Result result;

    @Mock
    Message msg;

    @Mock
    private CdsService cdsService;

    @Mock
    private CqnSelect cqnSelect;
    @Mock
    private AuditLogHelper<ComplaintTypeConfigurations> auditLogHelper;

    @Mock
    private ComplaintTypeConfigurationHandler crHandler;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeConfigurations.class);
        complaintType.setId("143");
        complaintType.setCode("code");
        complaintType.setIndividualComplaintType(false);
    }

    @Test
    public void deactiveComplainTypesTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintType.setIsActiveEntity(true);
        complaintType.setIsActive(true);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        handler.deactivateComplaintTypeConfigurations(context);
    }

    @Test
    public void deactiveComplainTypesTestIsActive() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintType.setIsActiveEntity(true);
        complaintType.setIsActive(false);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        handler.deactivateComplaintTypeConfigurations(context);
    }

    @Test
    public void deactiveComplainTypesTestIsInActive() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        complaintType.setIsActiveEntity(false);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        handler.deactivateComplaintTypeConfigurations(context);
    }
    @Test
    public void afterDeactivateComplaintTypesTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeConfigurations.class)).thenReturn(complaintType);
        handler.afterDeactivateComplaintTypes(context);
    }
}