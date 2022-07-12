package com.sap.ic.cmh.masterdata.businesspartner.controller;

import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.ArrayList;
import java.util.List;



public class BusinessPartnerControllerTest {

    @InjectMocks
    @Autowired
    BusinessPartnerController businessPartnerController;

    @Mock
    private BusinessPartnerServiceImpl businessPartnerService;

    static List<BusinessPartnerRequest> businessPartnerRequest;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        BusinessPartnerRequest request = new BusinessPartnerRequest();
        request.setBusinessPartner("10000001");
        BusinessPartnerRequest request1 = new BusinessPartnerRequest();
        request1.setBusinessPartner("10000002");
        BusinessPartnerRequest request2 = new BusinessPartnerRequest();
        request2.setBusinessPartner("10000003");

        businessPartnerRequest = new ArrayList<BusinessPartnerRequest>();
        businessPartnerRequest.add(request);
        businessPartnerRequest.add(request1);
        businessPartnerRequest.add(request2);

    }

    @Test
    public void testDeleteBusinessPartnerLists() throws Exception {
        businessPartnerController.deleteBusinessPartnerLists(businessPartnerRequest);
    }

    @Test
    public void testDeleteBusinessPartnerListNullTest() {
        businessPartnerController.deleteBusinessPartnerLists(null);
    }

}