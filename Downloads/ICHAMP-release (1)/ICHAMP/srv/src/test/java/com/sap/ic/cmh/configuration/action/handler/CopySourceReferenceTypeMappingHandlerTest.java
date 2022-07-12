package com.sap.ic.cmh.configuration.action.handler;


import cds.gen.configurationservice.CopySourceReferenceTypeMappingsContext;
import cds.gen.configurationservice.SourceReferenceTypeMappings;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CopySourceReferenceTypeMappingHandlerTest {
    @InjectMocks
    CopySourceReferenceTypeMappingHandler handler;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    DraftService draftService;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Result result;
    @Mock
    Message msg;
    private SourceReferenceTypeMappings sourceReferenceTypeMapping;
    @Mock
    private CopySourceReferenceTypeMappingsContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;
    @Mock
    Messages messages;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        sourceReferenceTypeMapping = Struct.create(SourceReferenceTypeMappings.class);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void copyTargetTypesTest() {
        Messages messages1 = Mockito.mock(Messages.class);
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(SourceReferenceTypeMappings.class)).thenReturn(sourceReferenceTypeMapping);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        when(messages1.success(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        handler.copySourceReferenceTypeMappings(context);
    }


}

