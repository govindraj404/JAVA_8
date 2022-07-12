package com.sap.ic.cmh.metering.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class UsageDocumentsDataTest {
    @InjectMocks
    UsageDocumentsData client;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void setMethodTest() {
        List< UsageDocument> l=new ArrayList<>();
        client.setUsage(l);
    }
    @Test
    public void getMethodTest() {
        client.getUsage();
    }

}
