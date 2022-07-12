package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ReactivateSourceReferenceTypeMappingsContext;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.SourceReferenceTypeMappingHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ReactivateSourceReferenceTypeMappingsHandlerTest {
    @InjectMocks
    ReactivateSourceReferenceTypeMappingsHandler handler;
    @Mock
    SourceReferenceTypeMappings sourceReferenceType;

    @Mock
    EventHandler eventHandler;

    @Mock
    Messages messages;

    @Mock
    PersistenceService db;

    @Mock
    ReactivateSourceReferenceTypeMappingsContext context;

    @Mock
    Result result;

    @Mock
    Message msg;

    @Mock
    private CdsService cdsService;

    @Mock
    private CqnSelect cqnSelect;
    @Mock
    AuditLogHelper<SourceReferenceTypeMappings> auditLogHelper;

    @Mock
    SourceReferenceTypeMappingHandler sourceReferenceTypeMappingHandler;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        sourceReferenceType = Struct.create(SourceReferenceTypeMappings.class);
        sourceReferenceType.setId("143");
    }

    @Test
    public void deactiveComplainTypesTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        sourceReferenceType.setIsActiveEntity(true);
        sourceReferenceType.setIsActive(true);
        when(result.single(SourceReferenceTypeMappings.class)).thenReturn(sourceReferenceType);
        handler.reactivateSourceReferenceTypeMappings(context);
    }

    @Test
    public void deactiveComplainTypesTestIsActive() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        sourceReferenceType.setIsActiveEntity(true);
        sourceReferenceType.setIsActive(false);
        when(result.single(SourceReferenceTypeMappings.class)).thenReturn(sourceReferenceType);
        handler.reactivateSourceReferenceTypeMappings(context);
    }

    @Test
    public void deactiveComplainTypesTestIsInActive() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        sourceReferenceType.setIsActiveEntity(false);
        when(result.single(SourceReferenceTypeMappings.class)).thenReturn(sourceReferenceType);
        handler.reactivateSourceReferenceTypeMappings(context);
    }

    @Test
    public void afterReactivateSourceReferenceTypeMappingsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(SourceReferenceTypeMappings.class)).thenReturn(sourceReferenceType);
        handler.afterReactivateSourceReferenceTypeMappings(context);
    }
}