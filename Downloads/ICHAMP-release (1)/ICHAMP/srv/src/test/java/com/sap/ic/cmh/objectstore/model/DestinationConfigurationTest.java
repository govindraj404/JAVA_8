package com.sap.ic.cmh.objectstore.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class DestinationConfigurationTest {

    @InjectMocks
    DestinationConfiguration config;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testMethod(){
        config.setAccessKeyId("testId");
        config.setAccountName("testName");
        config.setAuthentication("Authentication");
        config.setBucket("Bucket");
        config.setProxyType("ProxyType");
        config.setType("Type");
        config.setSecretAccessKey("SecretAccessKey");
        config.setuRL("url");
        config.setName("name");
        config.setSasToken("SasToken");

        config.getAccessKeyId();
        config.getAccountName();
        config.getAuthentication();
        config.getBucket();
        config.getProxyType();
        config.getType();
        config.getSecretAccessKey();
        config.getuRL();
        config.getName();
        config.getSasToken();

    }
}
