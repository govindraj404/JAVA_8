/* package com.sap.ic.cmh.organisation;
import org.mockito.*;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import java.util.ArrayList;
import java.util.List;
import static org.mockito.ArgumentMatchers.any;
import static org.junit.Assert.assertEquals;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.organisation.persistency.BusinessPartnerDao;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.junit4.SpringRunner;

import cds.gen.complainthandlingservice.BusinessPartner;


public class BusinessPartnerDaoTest {

    @InjectMocks @Autowired
    BusinessPartnerDao businessPartnerDao;

    @Mock
    protected PersistenceService mockDb;

    @Mock
    Result result;

    private BusinessPartner businessPartner;
    private List<BusinessPartner> businessPartnerList = new ArrayList<>();

    @Before
    public void before() {
        MockitoAnnotations.openMocks(this);
        businessPartner = Struct.create(BusinessPartner.class);
        businessPartner.setId("536fr-56hjf-987bhx-jok89");
        businessPartner.setDepartment("Department");
        businessPartner.setExternalId("DEALER01");
        businessPartner.setName("John");
        businessPartnerList.add(businessPartner);
    }

    @Test
    public void testGetBusinessPartner() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(BusinessPartner.class)).thenReturn(businessPartnerList);
        assertEquals(businessPartner, businessPartnerDao.getBusinessPartner("536fr-56hjf-987bhx-jok89"));
    }

    @Test
    public void testGetBusinessPartnerNull() {
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.listOf(BusinessPartner.class)).thenReturn(null);
        assertEquals(null, businessPartnerDao.getBusinessPartner("536fr-56hjf-987bhx-jok89"));
    }

}
 */