package com.sap.ic.cmh.masterdata.purchaseorganization.controller;

import com.sap.ic.cmh.masterdata.purchaseorganization.model.PurchaseOrganizationRequest;
import com.sap.ic.cmh.masterdata.purchaseorganization.service.PurchaseOrganizationService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class PurchaseOrganizationControllerTest {

    @InjectMocks
    @Autowired
    PurchaseOrganizationController controller;

    @Mock
    private PurchaseOrganizationService purchaseOrganizationService;


    private List<PurchaseOrganizationRequest> purchaseOrganizationRequest = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        PurchaseOrganizationRequest request = new PurchaseOrganizationRequest();
        request.setPurchaseOrganization("Organization");
        request.setRecordNo("F1234");

        purchaseOrganizationRequest.add(request);
    }

    @Test
    public void testDeletePurchaseOrganizationLists() {
        controller.deletePurchaseOrganization(purchaseOrganizationRequest);
    }

    @Test
    public void testDeletePurchaseOrganizationListNullTest() {
        controller.deletePurchaseOrganization(null);
    }
}
