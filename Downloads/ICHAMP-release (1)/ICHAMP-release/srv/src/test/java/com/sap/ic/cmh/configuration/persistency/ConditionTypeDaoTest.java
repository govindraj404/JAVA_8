package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.BusinessObjectConfigurations_;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.ConditionTypes_;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
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

public class ConditionTypeDaoTest {
    @InjectMocks
    ConditionTypeDao dao;
    @Mock
    PersistenceService db;
    @Mock
    Result result;
    @Mock
    Runnable run;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    ConditionTypes conditionTypes;
    private List<ConditionTypes_> boList = new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        conditionTypes =  Struct.create(ConditionTypes.class);
        conditionTypes.setConditionType("100");
        conditionTypes.setDestination("100");
        conditionTypes.setItemTypeCode("120");
        conditionTypes.setDestination("1");
        conditionTypes.setIdentifier(2);
        conditionTypes.setId("100");
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getConditionTypesTest(){
        dao.getConditionTypes();
    }
    @Test
    public void getConditionTypesBasedOnDestinationAndItemTypeTest(){
        dao.getConditionTypesBasedOnDestinationAndItemType(conditionTypes
                .getDestination(), conditionTypes.getItemTypeCode());
    }
    @Test
    public void testGetConditionTypesDetail() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(ConditionTypes_.class)).thenReturn(boList);
        dao.getConditionTypesDetail(conditionTypes.getId());
    }
}
