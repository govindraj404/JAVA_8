package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ServiceMaterials;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import cds.gen.configurationservice.ServiceMaterialUnits;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ServiceMaterialUnitDaoTest {
    @InjectMocks
    ServiceMaterialUnitDao dao;
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
    ServiceMaterialUnits serviceMaterialUnits;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        serviceMaterialUnits =  Struct.create(ServiceMaterialUnits.class);
        serviceMaterialUnits.setUnitCode("TAG");
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getServiceMaterialUnitsTest(){
        dao.getServiceMaterialUnits();
    }

    @Test
    public void getServiceMaterialUnitsBasedOnServiceMaterialTest(){
        dao.getServiceMaterialUnitsBasedOnServiceMaterial(serviceMaterialUnits.getId());
    }

    @Test
    public void getServiceMaterialUnitBasedOnMaterialIdAndUnitTest(){
        dao.getServiceMaterialUnitBasedOnMaterialIdAndUnit(serviceMaterialUnits.getId(), serviceMaterialUnits.getUnitCode());
    }
}
