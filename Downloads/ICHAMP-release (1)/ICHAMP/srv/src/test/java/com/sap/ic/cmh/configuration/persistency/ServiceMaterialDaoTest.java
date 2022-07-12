package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.DestinationConfigurations_;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;
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

public class ServiceMaterialDaoTest {
    @InjectMocks
    ServiceMaterialDao dao;
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
    ServiceMaterials serviceMaterials;
    private List<ServiceMaterials_> boList = new ArrayList<>();
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        serviceMaterials =  Struct.create(ServiceMaterials.class);
        serviceMaterials.setIdentifier(10);
        serviceMaterials.setDestination("100");
         serviceMaterials.setDescription("2");
        serviceMaterials.setId("100");
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getServiceMaterialsTest(){
        dao.getServiceMaterials();
    }
    @Test
    public void getServiceMaterialsBasedOnDestinationAndSubItemTypeTest(){
        dao.getServiceMaterialsBasedOnDestinationAndSubItemType(serviceMaterials.getItemTypeCode(), serviceMaterials.getSubItemTypeCode());
    }
    @Test
    public void getServiceMaterialsBasedOnDestinationSubItemTypeAndItemTypeTest(){
        dao.getServiceMaterialsBasedOnDestinationSubItemTypeAndItemType(serviceMaterials.getItemTypeCode(), serviceMaterials.getSubItemTypeCode(),serviceMaterials.getSubItemTypeCode());
    }

    @Test
    public void getServiceMaterialsBasedOnSubItemTypeTest(){
        dao.getServiceMaterialsBasedOnSubItemType(serviceMaterials.getSubItemTypeCode());
    }
    @Test
    public void testGetServiceMaterialsDetail() {
        Mockito.when(db.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(ServiceMaterials_.class)).thenReturn(boList);
        dao.getServiceMaterialsDetail(serviceMaterials.getId());
    }
}
