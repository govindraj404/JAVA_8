package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.DestinationConfigurations;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.action.context.CopyDestinationConfigurationContext;
import com.sap.ic.cmh.network.service.DestinationService;
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

public class CopyDestinationConfigurationHandlerTest {
    @Mock
    DraftService draftService;
    @InjectMocks
    CopyDestinationConfigurationHandler handler;
    @Mock
    DestinationService destinationService;

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
    private DestinationConfigurations businessObjectConfigurations;
    @Mock
    private CopyDestinationConfigurationContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        businessObjectConfigurations = Struct.create(DestinationConfigurations.class);
        businessObjectConfigurations.setId("100");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void copyDestinationConfigurationsTest() {
        when(context.getCqn()).thenReturn(cqnSelect);
        when(context.getService()).thenReturn(cdsService);
        when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
        when(result.single(DestinationConfigurations.class)).thenReturn(businessObjectConfigurations);
        when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
        handler.copyDestinationConfigurations(context);
    }

}