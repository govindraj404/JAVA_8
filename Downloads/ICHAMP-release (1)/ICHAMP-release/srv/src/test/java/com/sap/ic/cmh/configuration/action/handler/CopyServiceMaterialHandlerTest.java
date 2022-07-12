package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ServiceMaterials;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.action.context.CopyServiceMaterialContext;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
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

public class CopyServiceMaterialHandlerTest {
    @Mock
    DraftService draftService;
    @InjectMocks
    CopyServiceMaterialHandler handler;
    @Mock
    DestinationService destinationService;
    @Mock
    ServiceMaterialUnitDao serviceMaterialUnitDao;
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
    private ServiceMaterials serviceMaterials;
    @Mock
    private CopyServiceMaterialContext context;
    @Mock
    private CdsService cdsService;
    @Mock
    private CqnSelect cqnSelect;
    @Mock
    private Result result1;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        serviceMaterials = Struct.create(ServiceMaterials.class);
        serviceMaterials.setId("100");
        serviceMaterials.setSubItemTypeCode("f");
        serviceMaterials.setDestination("eee");
        serviceMaterials.setItemTypeCode("rerw");
        serviceMaterials.setServiceMaterial("12");
        serviceMaterials.setDescription("values");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }


}
