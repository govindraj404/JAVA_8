
package com.sap.ic.cmh.configuration.action.handler;


import cds.gen.configurationservice.CopyTargetReferenceTypeMappingsContext;
import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypes;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.TargetReferenceTypeDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


public class CopyTargetReferenceTypeMappingHandlerTest {

    @InjectMocks
    CopyTargetReferenceTypeMappingHandler handler;

    @Mock
    DraftService draftService;
    @Mock
    TargetReferenceTypeDao targetReferenceTypeDao;
    @Mock
    Messages messages;
    @Mock
    Result result;
    @Mock
    CopyTargetReferenceTypeMappingsContext context;
    @Mock
    CdsService cdsService;
    @Mock
    CqnSelect cqnSelect;
    @Mock
    TargetReferenceTypeMappings copyTargetReferenceTypeMappings;
    CopyTargetReferenceTypeMappingsContext copyTargetReferenceTypeMappingsContext;

    @Mock
    Message msg;
    @Mock
    TargetReferenceTypeMappings targetReferenceTypeMappings;

    @Mock
    protected PersistenceService mockDb;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        copyTargetReferenceTypeMappingsContext = Struct.create(CopyTargetReferenceTypeMappingsContext.class);
        copyTargetReferenceTypeMappings = Struct.create(TargetReferenceTypeMappings.class);

    }

    @Test
    public void CopyTargetReferenceTypeMappingEmptyRefrenceStreamTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(CopyTargetReferenceTypeMappingsContext.class)).thenReturn(copyTargetReferenceTypeMappingsContext);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(targetReferenceTypeMappings);
        when(targetReferenceTypeDao.getTargetReferenceTypesBasedOnTargetMapping(targetReferenceTypeMappings.getId())).thenReturn(result);
        when(messages.success(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        handler.copyTargetReferenceTypeMappings(context);


    }


    @Test
    public void CopyTargetReferenceTypeMappingTest() {

        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(CopyTargetReferenceTypeMappingsContext.class)).thenReturn(copyTargetReferenceTypeMappingsContext);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(TargetReferenceTypeMappings.class)).thenReturn(targetReferenceTypeMappings);
        when(targetReferenceTypeDao.getTargetReferenceTypesBasedOnTargetMapping(targetReferenceTypeMappings.getId())).thenReturn(result);
        when(result.rowCount()).thenReturn(2L);
        List<TargetReferenceTypes> list = new ArrayList<>();
        TargetReferenceTypes t = Struct.create(TargetReferenceTypes.class);
        t.setTargetTypeId("12");
        t.setDestinationSystem("test");
        list.add(t);
        when(result.streamOf(TargetReferenceTypes.class)).thenReturn(list.stream());
        when(messages.success(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        handler.copyTargetReferenceTypeMappings(context);


    }


}