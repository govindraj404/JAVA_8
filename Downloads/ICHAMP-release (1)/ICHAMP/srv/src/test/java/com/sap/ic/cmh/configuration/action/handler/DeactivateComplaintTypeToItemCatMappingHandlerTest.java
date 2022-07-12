package com.sap.ic.cmh.configuration.action.handler;
import cds.gen.configurationservice.*;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.configuration.handler.ComplaintTypeToItemCatMappingHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
public class DeactivateComplaintTypeToItemCatMappingHandlerTest {
    @InjectMocks
     DeactivateComplaintTypeToItemCatMappingHandler handler;
    @Mock
    DeactivateComplaintTypeToItemCategoryMappingsContext context;
    private
    ComplaintTypeToItemCategoryMappings compTypeToItemCatMap;
    @Mock
    CqnSelect cqnSelect;

    @Mock
    private CdsService cdsService;

    @Mock
    Result result;

    @Mock
    Messages messages;
    @Mock
    protected PersistenceService db;

    @Mock
    AuditLogHelper<ComplaintTypeToItemCategoryMappings> auditLogHelper;

    @Mock
    ComplaintTypeToItemCatMappingHandler complaintTypeToItemCatMappingHandler;

    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);
        compTypeToItemCatMap=Struct.create(ComplaintTypeToItemCategoryMappings.class);

    }

    @Test
    public void onDeactivateComplaintTypeToItemCatMapsTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeToItemCategoryMappings.class)).thenReturn(compTypeToItemCatMap);
        compTypeToItemCatMap.setIsActiveEntity(true);
        compTypeToItemCatMap.setIsActive(false);
        handler.onDeactivateComplaintTypeToItemCatMaps(context);
    }
    @Test
    public void onDeactivateComplaintTypeToItemCatMapsIsActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeToItemCategoryMappings.class)).thenReturn(compTypeToItemCatMap);
        compTypeToItemCatMap.setIsActiveEntity(true);
        compTypeToItemCatMap.setIsActive(true);
        handler.onDeactivateComplaintTypeToItemCatMaps(context);
    }
    @Test
    public void onDeactivateComplaintTypeToItemCatMapsIsInActiveTest()
    {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeToItemCategoryMappings.class)).thenReturn(compTypeToItemCatMap);
        compTypeToItemCatMap.setIsActiveEntity(false);
        handler.onDeactivateComplaintTypeToItemCatMaps(context);
    }
    @Test
    public void afterDeactivateComplaintTypeToItemCatMapsTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ComplaintTypeToItemCategoryMappings.class)).thenReturn(compTypeToItemCatMap);
        handler.afterDeactivateComplaintTypeToItemCatMaps(context);
    }
}
