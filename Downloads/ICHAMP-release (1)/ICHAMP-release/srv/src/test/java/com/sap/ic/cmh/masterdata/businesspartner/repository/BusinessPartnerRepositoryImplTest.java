package com.sap.ic.cmh.masterdata.businesspartner.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import com.sap.cds.Result;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.ql.Update;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class BusinessPartnerRepositoryImplTest {
    @InjectMocks
    @Autowired
    private BusinessPartnerRepositoryImpl businessPartnerRepositoryImpl;


    @Mock
    private PersistenceService mockDb;

    @Mock
    Result result;

    private  List<String> businessPartnersList;
    private  List<String> businessPartnersIds = new ArrayList<>();
    private List<BusinessPartners> businessPartnerList = new ArrayList<>();
    private BusinessPartners businessPartner;
    private List<Complaints> complaintsList = new ArrayList<>();
    private Complaints complaints;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        businessPartnersList = new ArrayList<>();
        businessPartnersList.add("100001");

        businessPartner = BusinessPartners.create();
        businessPartner.setId("1000");
        businessPartner.setBusinessPartnerNumber("100002");
        businessPartnerList.add(businessPartner);

        businessPartnersIds.add("100");
        complaints = Complaints.create();
        complaints.setSupplierId("100");
        complaintsList.add(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
    }

    @Test
    public void testDeleteBusinessPartner() {
        CqnInsert cqnInsert = Insert.into(BusinessPartners_.class).entry(businessPartner);
        mockDb.run(cqnInsert);
        businessPartnerRepositoryImpl.deleteBusinessPartnerList(businessPartnersList);
    }

    @Test
    public void testBusinessPartnersMap() {
        Map<String, String> businessPartnerMap = new HashMap<>();
        businessPartnerMap.put("1000", "100002");
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnerList);
        assertEquals(businessPartnerMap, businessPartnerRepositoryImpl.getBusinessPartnersMap(businessPartnersIds));

    }

    @Test
    public void testGetActiveComplaintsInBusinessPartner() {

        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Complaints.class)).thenReturn(complaintsList);
        assertEquals(activeComplaintsList,businessPartnerRepositoryImpl.getActiveComplaintsInBusinessPartner(businessPartnersIds));

    }
     @Test
     public void testUpdateBusinessPartner() {
      CqnInsert cqnInsert = Insert.into(BusinessPartners_.class).entry(businessPartner);
      mockDb.run(cqnInsert);
      businessPartner.setIsMarkedForDeletion(true);
      businessPartnerRepositoryImpl.updateBusinessPartner(businessPartner);
      
  }

	@Test
	public void testGetBusinessPartners() {
		businessPartnerRepositoryImpl.getBusinessPartners("adfstg");
	}

	@Test
	public void testGetBusinessPartnerBasedOnNumber() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		businessPartnerRepositoryImpl.getBusinessPartnersBasedOnNumber(businessPartner.getBusinessPartnerNumber());
	}
    @Test
    public void testCheckIfCustomerCodeExists() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        businessPartnerRepositoryImpl.checkIfCustomerCodeExists(businessPartner.getCustomerCode());
    }
    @Test
    public void testCheckIfVendorCodeExists() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        businessPartnerRepositoryImpl.checkIfVendorCodeExists(businessPartner.getVendorCode());
    }
}
