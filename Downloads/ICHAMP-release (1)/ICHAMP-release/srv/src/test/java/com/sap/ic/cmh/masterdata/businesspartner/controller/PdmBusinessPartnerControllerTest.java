package com.sap.ic.cmh.masterdata.businesspartner.controller;

import com.sap.ic.cmh.masterdata.businesspartner.model.PdmBusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PdmBusinessPartnerControllerTest {
    @InjectMocks
    PdmBusinessPartnerController controller;
    @Mock
    private BusinessPartnerService businessPartnerService;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void sendMeteringDataTest() {
        PdmBusinessPartnerRequest req=new PdmBusinessPartnerRequest();
        controller.deleteBusinessPartner(req);
    }
}

