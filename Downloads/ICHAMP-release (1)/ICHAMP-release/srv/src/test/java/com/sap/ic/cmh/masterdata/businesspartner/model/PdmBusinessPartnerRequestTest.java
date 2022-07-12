package com.sap.ic.cmh.masterdata.businesspartner.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PdmBusinessPartnerRequestTest {
    @InjectMocks
    PdmBusinessPartnerRequest request;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public  void setMethodTest(){
        request.setBusinessPartner("test");
        request.setRecordNo("test");
    }

    @Test
    public  void getMethodTest(){
        request.getBusinessPartner();
        request.getRecordNo();
    }
}
