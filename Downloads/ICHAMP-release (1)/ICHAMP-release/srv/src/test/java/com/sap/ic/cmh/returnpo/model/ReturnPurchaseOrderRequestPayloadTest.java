package com.sap.ic.cmh.returnpo.model;

import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class ReturnPurchaseOrderRequestPayloadTest {
    @InjectMocks
    ReturnPurchaseOrderRequestPayload payLoad;
    @Before
    public void beforeClass()  {
        MockitoAnnotations.openMocks(this);}
    @Test
    public void setMethodTest(){
        payLoad.setRpoBackendStatus("test");
        payLoad.setRpoNumber("test");
        payLoad.setRpoBackendStatus("test");
    }

    @Test
    public void getMethodTest(){
        payLoad.getRpoBackendStatus();
        payLoad.getRpoNumber();
        payLoad.getRpoBackendStatus();
    }
    @Test
    public void testToString(){
        payLoad.toString();
    }
}
