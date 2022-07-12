package com.sap.ic.cmh.utils;

import io.pivotal.cfenv.core.CfEnv;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.Map;

public class PlatformUtilTest {

    @InjectMocks
    @Autowired
    PlatformUtil platformUtil;
    @Mock
    PlatformUtil pf;

    String vcap;
    String vcapUserProvided;

    PlatformUtil platform = new PlatformUtil();
    PlatformUtil platformSpy = Mockito.spy(platform);

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        vcap="{managed-hana:[{credentials:{user:test}}]}";
        vcapUserProvided="{user-provided:[{name:managed-hana,credentials:{user:test}}]}";
    }

    @Test
    public void testGetCredentials() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getCredentials("managed-hana");
    }

    @Test
    public void testGetCredentialsJsonException() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getCredentials("invalid_key");
    }

    // @Test
    // public void testGetCredentialsException() {
    //     platformUtil.getCredentials("");
    // }

    @Test
    public void testGetUserProvidedInstanceCredentials() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcapUserProvided);
        platformSpy.getUserProvidedInstanceCredentials("managed-hana");
    }

    @Test
    public void testGetUserProvidedInstanceCredentialsJsonException() {
        vcapUserProvided="{invalid_key:[ ]}";
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcapUserProvided);
        platformSpy.getUserProvidedInstanceCredentials("invalid_key");
    }

    // @Test
    // public void testGetUserProvidedInstanceCredentialsException() {
    //     platformSpy.getUserProvidedInstanceCredentials("managed-hana");
    // }

    @Test
    public void testGetServiceCredentials(){
        Map<String, Object> map=new HashMap<>();
        map.put("region","region");
        map.put("metering_url","test");
        map.put("serviceName","managed-hana");
        map.put("tenantid","1231231231242134");
        Mockito.when(pf.getServiceCredentials("managed-hana")).thenReturn(map);
        Mockito.when(pf.getEnvironmentDetail()).thenReturn(vcapUserProvided);
        Mockito.when(pf.getEnvironmentDetail()).thenReturn(vcap);
        pf.getServiceCredentials("managed-hana");
        pf.getServiceCredentials("test");
    }

    @Test
    public void testGetCredentialsNull() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getCredentials(null);
    }

    @Test
    public void testGetCredentialsExp() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getCredentials("{[/");
    }

    @Test
    public void testgetUserProvidedInstanceCredentialsNull() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getUserProvidedInstanceCredentials(null);
    }

    @Test
    public void testgetUserProvidedInstanceCredentialsExp() {
        Mockito.when(platformSpy.getEnvironmentDetail()).thenReturn(vcap);
        platformSpy.getUserProvidedInstanceCredentials("%454^^6");
    }
    @Test(expected = Exception.class)
    public void testgetServiceCredentials() {
        platformSpy.getServiceCredentials("Credentials");
    }
}
