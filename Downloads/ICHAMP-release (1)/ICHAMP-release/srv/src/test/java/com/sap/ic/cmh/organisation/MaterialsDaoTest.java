/* package com.sap.ic.cmh.organisation;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import static org.mockito.ArgumentMatchers.any;
import static org.junit.Assert.assertEquals;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.organisation.persistency.MaterialsDao;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.junit4.SpringRunner;

import cds.gen.complainthandlingservice.Materials;


public class MaterialsDaoTest {

    @InjectMocks @Autowired
    MaterialsDao materialsDao;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    Result result;

    private Materials material;
    private List<Materials> materialList = new ArrayList<>();

    @Before
    public void before() {
        MockitoAnnotations.openMocks(this);
        material = Struct.create(Materials.class);
        material.setId("536fr-56hjf-987bhx-jok89");
        material.setMaterialExternalId("ExternalID");
        BigDecimal b1 = new BigDecimal("1");
        material.setQuantity(b1);
        material.setUnitCode("EA");
        materialList.add(material);
    }

    @Test
    public void testGetBusinessPartner() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(Materials.class)).thenReturn(materialList);
        assertEquals(material, materialsDao.getMaterial("536fr-56hjf-987bhx-jok89"));
    }

    @Test
    public void testGetBusinessPartnerNull() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(Materials.class)).thenReturn(null);
        assertEquals(null, materialsDao.getMaterial("536fr-56hjf-987bhx-jok89"));
    }

}
 */