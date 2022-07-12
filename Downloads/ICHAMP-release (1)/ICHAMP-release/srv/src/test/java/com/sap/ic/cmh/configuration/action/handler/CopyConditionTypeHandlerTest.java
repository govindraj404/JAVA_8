package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ConditionTypes;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.action.context.CopyConditionTypeContext;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CopyConditionTypeHandlerTest {
    @Mock
    DraftService draftService;
    @InjectMocks
    CopyConditionTypeHandler handler;
    @Mock
    MessageKeys keys;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    CqnInsert cqnInsert;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Messages messages;
    private ConditionTypes businessObjectConfigurations;
    @Mock
    private CopyConditionTypeContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        businessObjectConfigurations = Struct.create(ConditionTypes.class);
        businessObjectConfigurations.setId("100");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void copyConditionTypeTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(ConditionTypes.class)).thenReturn(businessObjectConfigurations);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        handler.copyConditionType(context);
    }

}
