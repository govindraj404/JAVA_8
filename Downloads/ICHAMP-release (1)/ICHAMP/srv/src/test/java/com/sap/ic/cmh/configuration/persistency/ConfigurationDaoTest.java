package com.sap.ic.cmh.configuration.persistency;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Addresses;
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
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ConfigurationDaoTest {
    @InjectMocks
    ConfigurationDao dao;
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
    MaterialMasterGeneralDatas materialMasterGeneralDatas;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        materialMasterGeneralDatas = Struct.create(MaterialMasterGeneralDatas.class);
        materialMasterGeneralDatas.setMaterialCode("100");
        materialMasterGeneralDatas.setDivision("100");
        materialMasterGeneralDatas.setMaterialGroup("120");
        materialMasterGeneralDatas.setMaterialType("1");
        materialMasterGeneralDatas.setEANCategary("2");
        materialMasterGeneralDatas.setId("100");
        when(db.run(any(CqnDelete.class))).thenReturn(result);
        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void getMaterialDataTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getMaterialData(materialMasterGeneralDatas.getMaterialCode());
    }

    @Test
    public void getPlantDataTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getPlantData("201");
    }

    @Test
    public void getSupplierDataTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getSupplierData("201");
    }

    @Test
    public void getPurchOrgDataTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getPurchOrgData("201");
    }

    @Test
    public void getCompanyCodesTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getCompanyCodes("201");
    }

    @Test
    public void findSupplierTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        // dao.findSupplier("201");
    }

    @Test
    public void findMaterialTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        // dao.findMaterial("201");
    }

    @Test
    public void findPlantTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        // dao.findPlant("201");
    }

    @Test
    public void findPurchaseOrganizationTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        // dao.findPurchaseOrganization("201");
    }

    @Test
    public void findCompanyCodeTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        // dao.findCompanyCode("201");
    }

    @Test
    public void getMaterialDataBasedOnCodeTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getMaterialDataBasedOnCode("201");
    }

    @Test
    public void getPlantDataBasedOnPlantTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getPlantDataBasedOnPlant("201");
    }

    @Test
    public void getSupplierDataBasedOnNumberTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getSupplierDataBasedOnNumber("201");
    }

    @Test
    public void getPersonResponsibleBasedOnNumberTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getPersonResponsibleBasedOnNumber("201");
    }

    @Test
    public void getPurchOrgDataBasedOnPurchOrgTest() {
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        dao.getPurchOrgDataBasedOnPurchOrg("201");
    }

}
