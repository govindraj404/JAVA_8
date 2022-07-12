package com.sap.ic.cmh.utils;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class XsuaaTokenTest {
    @InjectMocks
    XsuaaToken service;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void getAccessTokenTest() throws IOException {
        Map<String,Object> map=new HashMap<>();
        map.put("saas_registry_url","test");
        map.put("url","test");
        map.put("clientid","test");
        map.put("clientsecret","test");
        service.getAccessToken("test","test","test");
    }
}
