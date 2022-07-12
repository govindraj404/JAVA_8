package com.sap.ic.cmh.configuration.action.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import cds.gen.configurationservice.*;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.TargetTypeConfigurationHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.sap.ic.cmh.auditlog.AuditLogHelper;

public class ReactivateTargetTypesHandlerTest {

    @InjectMocks
    ReactivateTargetTypesHandler handler;

    @Mock
    TargetTypes targetType;

    @Mock
    EventHandler eventHandler;

    @Mock
    Messages messages;

    @Mock
    PersistenceService db;

    @Mock
    ReactivateTargetTypesContext context;

    @Mock
    Result result;

    @Mock
    Message msg;

    @Mock
    private CdsService cdsService;

    @Mock
    private CqnSelect cqnSelect;

    @Mock
    AuditLogHelper<TargetTypes> auditLogHelper;

    @Mock
    TargetTypeConfigurationHandler targetTypeHandler;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        targetType = Struct.create(TargetTypes.class);
        targetType.setId("143");
        targetType.setCode("code");
    }

    @Test
    public void reactivateTargetTypesTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        targetType.setIsActiveEntity(true);
        targetType.setIsActive(true);
        when(result.single(TargetTypes.class)).thenReturn(targetType);
        handler.reactivateTargetTypes(context);
    }

    @Test
    public void reactivateTargetTypesTestElse() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        targetType.setIsActiveEntity(true);
        targetType.setIsActive(false);
        when(result.single(TargetTypes.class)).thenReturn(targetType);
        handler.reactivateTargetTypes(context);
    }

    @Test
    public void reactivateTargetTypesTestElse1() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        targetType.setIsActiveEntity(false);
        when(result.single(TargetTypes.class)).thenReturn(targetType);
        handler.reactivateTargetTypes(context);
    }

    @Test
    public void afterReactivateTargetTypesTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetTypes.class)).thenReturn(targetType);
        handler.afterReactivateTargetTypes(context);
    }
}