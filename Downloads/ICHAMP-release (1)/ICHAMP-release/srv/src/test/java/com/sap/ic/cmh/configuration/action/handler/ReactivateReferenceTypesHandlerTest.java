package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ReactivateReferenceTypesContext;
import cds.gen.configurationservice.ReferenceTypes;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ReferenceTypeHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ReactivateReferenceTypesHandlerTest {

    @InjectMocks
    ReactivateReferenceTypesHandler handler;

    @Mock
    ReferenceTypes referenceTypes;

    @Mock
    ReactivateReferenceTypesContext context;

    @Mock
    Messages messages;

    @Mock
    PersistenceService db;

    @Mock
    Result result;

    @Mock
    private CdsService cdsService;

    @Mock
    private CqnSelect cqnSelect;
    @Mock
    AuditLogHelper<ReferenceTypes> auditLogHelper;
    @Mock
    ReferenceTypeHandler referenceTypeHandler;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        referenceTypes = Struct.create(ReferenceTypes.class);
        referenceTypes.setId("143");
        referenceTypes.setCode("code");
    }

    @Test
    public void reactiveReferenceType() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        referenceTypes.setIsActiveEntity(true);
        referenceTypes.setIsActive(true);
        when(result.single(ReferenceTypes.class)).thenReturn(referenceTypes);
        handler.reactivateReferenceTypes(context);
    }

    @Test
    public void reactiveReferenceTypeTestElse() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        referenceTypes.setIsActiveEntity(true);
        referenceTypes.setIsActive(false);
        when(result.single(ReferenceTypes.class)).thenReturn(referenceTypes);
        handler.reactivateReferenceTypes(context);
    }

    @Test
    public void reactiveReferenceTypeTestElse1() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        referenceTypes.setIsActiveEntity(false);
        when(result.single(ReferenceTypes.class)).thenReturn(referenceTypes);
        handler.reactivateReferenceTypes(context);
    }

    @Test
    public void afterDeactivateComplaintChannelsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ReferenceTypes.class)).thenReturn(referenceTypes);
        handler.afterReactivateReferenceType(context);
    }
}