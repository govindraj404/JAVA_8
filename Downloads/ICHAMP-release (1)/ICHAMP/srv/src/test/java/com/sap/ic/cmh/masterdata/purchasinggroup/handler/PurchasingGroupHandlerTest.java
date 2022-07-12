package com.sap.ic.cmh.masterdata.purchasinggroup.handler;

import cds.gen.masterdataservice.PurchasingGroups;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.purchasinggroup.service.PurchasingGroupService;
import com.sap.ic.cmh.masterdata.purchasinggroup.validation.PurchasingGroupValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.Mockito.when;


public class PurchasingGroupHandlerTest {

    @InjectMocks
    @Autowired
    PurchasingGroupHandler purchasingGroupHandler;
    @Mock
    PurchasingGroupService service;
    @Mock
    private PurchasingGroupValidator purchasingGroupValidator;

    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    CdsUpdateEventContext updateContextMock;

    private PurchasingGroups purchasingGroups;
    
    @Mock
    Messages messages;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        purchasingGroups = Struct.create(PurchasingGroups.class);
        purchasingGroups.setCode("code");
        purchasingGroups.setDescription("Description");
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void testCreatePurchasingGroups()  {
        Optional<PurchasingGroups> emptyOpt = Optional.empty();

        purchasingGroupHandler.beforePurchasingGroupOnCreate(createContextMock, purchasingGroups);
    }

    @Test
    public void testUpdatePurchasingGroups()  {
        Optional<PurchasingGroups> emptyOpt = Optional.of(purchasingGroups);
        purchasingGroupHandler.validatePurchasingGroupOnUpdate(updateContextMock, purchasingGroups);
    }
    @Test
    public void testSubItemTypeNull () {
        when(service.fetchPurchasingGroupDetails(purchasingGroups.getCode())).thenReturn(null);
        purchasingGroupHandler.onPurchasingGroupOnCreate(createContextMock,purchasingGroups );
    }

}
