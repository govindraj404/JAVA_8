package com.sap.ic.cmh.masterdata.purchaseorganization.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class PurchaseOrganizationRepositoryImplTest {

    @InjectMocks
    @Autowired
    PurchaseOrganizationRepositoryImpl purchaseOrganisationRepositoryImpl;

    @Mock
    private PersistenceService mockDb;

    @Mock
    Result result;

    private List<PurchaseOrganizations> purchaseOrganizationsList = new ArrayList<>();

    private  List<String> purchaseOrganizationsIds = new ArrayList<>();

    private PurchaseOrganizations purchaseOrganizations;

    private List<Complaints> complaintsList = new ArrayList<>();
    private Complaints complaints;
    private String purchaseOrganizationsId;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        purchaseOrganizationsId = "100001";
        purchaseOrganizations = PurchaseOrganizations.create();
        purchaseOrganizations.setId("100001");
        purchaseOrganizations.setPurchaseOrganization("Organization");

        purchaseOrganizationsList.add(purchaseOrganizations);
        purchaseOrganizationsIds.add("100001");

        complaints = Complaints.create();
        complaints.setComplaintStatusCode("CLSD");
        complaints.setPurchasingOrganizationId("100");
        complaintsList.add(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }
    @Test
    public void testPurchaseOrganizationsMap() {
        Map<String, String> plantMap = new HashMap<>();
        plantMap.put("100001", "Organization");
        when(result.listOf(PurchaseOrganizations.class)).thenReturn(purchaseOrganizationsList);
        assertEquals(plantMap, purchaseOrganisationRepositoryImpl.getPurchaseOrganizationMap(purchaseOrganizationsIds));
    }

    @Test
    public void testDeletePurchaseOrganizations() {
        CqnInsert cqnInsert = Insert.into(PurchaseOrganizations_.class).entry(purchaseOrganizations);
        mockDb.run(cqnInsert);
        purchaseOrganisationRepositoryImpl.deletePurchaseOrganizationList(purchaseOrganizationsIds);
    }

    @Test
    public void testGetActiveComplaintsInPurchaseOrganizations() {

        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Complaints.class)).thenReturn(complaintsList);
        assertEquals(activeComplaintsList,purchaseOrganisationRepositoryImpl.getActiveComplaintsInPurchaseOrganization(purchaseOrganizationsIds));

    }
    @Test
    public void testFetchReasonBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        purchaseOrganisationRepositoryImpl.fetchPurchaseOrganization("d");

    }
}
