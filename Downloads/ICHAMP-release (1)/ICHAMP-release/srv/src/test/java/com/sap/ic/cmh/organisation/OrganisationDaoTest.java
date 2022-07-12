/* package com.sap.ic.cmh.organisation;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.junit.Assert.assertEquals;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.organisation.persistency.OrganisationDao;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.junit4.SpringRunner;

import cds.gen.complainthandlingservice.OrgData;
import cds.gen.db.ic.cmh.orgdata.Organisation;


public class OrganisationDaoTest {

    @InjectMocks @Autowired
    OrganisationDao orgDao;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    Result result;

    private Organisation org;
    private List<Organisation> orgList = new ArrayList<>();

    @Before
    public void before() {
        MockitoAnnotations.openMocks(this);
        org = Struct.create(Organisation.class);
        org.setId("345fr-56hjf-987bhx-jok89");
        org.setMaterialId("MaterialID");
        org.setSupplierId("supplierId");
        org.setPlantCode("1000");
        orgList.add(org);
    }

    @Test
    public void testGetBusinessPartner() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(Organisation.class)).thenReturn(orgList);
        assertEquals(org, orgDao.getOrganisation("345fr-56hjf-987bhx-jok89"));
    }

    @Test
    public void testGetBusinessPartnerNull() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(Organisation.class)).thenReturn(null);
        assertEquals(null, orgDao.getOrganisation("345fr-56hjf-987bhx-jok89"));
    }
}
 */