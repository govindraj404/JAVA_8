package com.sap.ic.cmh.masterdata.subitemtype.handler;

import cds.gen.masterdataservice.SubItemTypes;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.subitemtype.service.SubItemTypeService;
import com.sap.ic.cmh.masterdata.subitemtype.validation.SubItemTypeValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class SubItemTypeHandlerTest {

    @InjectMocks
    @Autowired
    public SubItemTypeHandler handler ;
    @Mock
    public SubItemTypeValidator validator;

    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CdsUpdateEventContext updateEventContext;
    @Mock
    CqnInsert cqnInsert;
    @Mock
    Messages messages;
    @Mock
    Message message;
    @Mock
    CdsService cdsService;
    @Mock
    SubItemTypeService subItemTypeService;
    
    private SubItemTypes subItemType;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        subItemType = Struct.create(SubItemTypes.class);

        subItemType.setCode("10000001");
        subItemType.setDescription("Henry-Strasse");
        subItemType.setItemTypeCode("70839");

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void testBeforeCreateSubItemType() {
        doNothing().when(validator).checkInputsSanitized(subItemType);
        handler.beforeSubItemTypeCreate(createContextMock,subItemType );
    }

    @Test
    public void testUpdateSubItemType () {
    	doNothing().when(validator).checkInputsSanitized(subItemType);
        handler.beforeSubItemTypeUpdate(updateEventContext,subItemType );
    }
    
    @Test
    public void testSubItemTypeNull () {
        when(subItemTypeService.fetchSubItemTypes(subItemType.getCode())).thenReturn(null);
        handler.onSubItemTypeCreate(createContextMock,subItemType );
    }

    @Test
    public void testSubItemTypeNotNull () {
        when(subItemTypeService.fetchSubItemTypes(subItemType.getCode())).thenReturn(subItemType);
        handler.onSubItemTypeCreate(createContextMock,subItemType );
    }
}
