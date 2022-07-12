package com.sap.ic.cmh.configuration.action.handler;


import cds.gen.configurationservice.DeactivateTargetReferenceTypeMappingsContext;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.TargetReferenceTypeMappingHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class DeactivateTargetReferenceTypeMappingsHandlerTest {
    @InjectMocks
    DeactivateTargetReferenceTypeMappingsHandler handler;


    @Mock
    DeactivateTargetReferenceTypeMappingsContext context;

    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    private
    TargetReferenceTypeMappings TargetRefTypeMapping;
    @Mock
    Result result;

    @Mock
    Messages messages;
    @Mock
    protected PersistenceService db;
    @Mock
    AuditLogHelper<TargetReferenceTypeMappings> auditLogHelper;

    @Mock
    private TargetReferenceTypeMappingHandler targetReferenceTypeMappingHandlerHandler;

    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);
        TargetRefTypeMapping=Struct.create(TargetReferenceTypeMappings.class);

    }

    @Test
    public void onDeactivateTargetRefTypeMappingsTest()
    {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(TargetRefTypeMapping);
        TargetRefTypeMapping.setIsActiveEntity(true);
        TargetRefTypeMapping.setIsActive(false);
        handler.onDeactivateTargetRefTypeMappings(context);
    }
    @Test
    public void onDeactivateTargetRefTypeMappingsIsActiveTest()
    {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(TargetRefTypeMapping);
        TargetRefTypeMapping.setIsActiveEntity(true);
        TargetRefTypeMapping.setIsActive(true);
        handler.onDeactivateTargetRefTypeMappings(context);
    }
    @Test
    public void onDeactivateTargetRefTypeMappingsIsInActiveTest() {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(TargetRefTypeMapping);
        TargetRefTypeMapping.setIsActiveEntity(false);
        handler.onDeactivateTargetRefTypeMappings(context);
    }
    @Test
    public void afterDeactivateTargetRefTypeMapTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(TargetRefTypeMapping);
        TargetRefTypeMapping.setIsActiveEntity(false);
        handler.afterDeactivateTargetRefTypeMap(context);
    }

}