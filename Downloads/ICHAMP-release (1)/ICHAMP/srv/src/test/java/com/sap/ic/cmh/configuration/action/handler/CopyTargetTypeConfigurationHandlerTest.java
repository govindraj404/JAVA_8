package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.CopyTargetTypesContext;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.TargetTypes;
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
import com.sap.ic.cmh.configuration.action.context.CopyServiceMaterialContext;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import com.sap.ic.cmh.network.service.DestinationService;
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

public class CopyTargetTypeConfigurationHandlerTest {

    @InjectMocks
    CopyTargetTypeConfigurationHandler handler;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    DraftService draftService;
    
    @Mock
    CqnInsert cqnInsert;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Messages messages;
    @Mock
    Message msg;
    private TargetTypes targetTypes;
    @Mock
    private CopyTargetTypesContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;
    @Mock
    private Result result1;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        targetTypes = Struct.create(TargetTypes.class);
        targetTypes.setId("100");
        targetTypes.setCode("f");
        targetTypes.setTargetDocumentCategoryCode("eee");
        targetTypes.setDescription("values");
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
        when(result.single(TargetTypes.class)).thenReturn(targetTypes);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        when(messages1.success(any(String.class),any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        handler.copyTargetTypes(context);
    }


}
