package com.sap.ic.cmh.tenancy.service;

import com.sap.ic.cmh.network.model.ResponseDto;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.utils.XsuaaToken;
import io.pivotal.cfenv.core.CfCredentials;
import io.pivotal.cfenv.core.CfEnv;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class TenantSubscriptionServiceTest {
    @InjectMocks
    TenantSubscriptionService  service;
    @Mock
    CfEnv env;
    @Mock
    XsuaaToken xsuaa;
    @Mock
    HttpService httpService;
    @Mock
    CfCredentials cfCredentials;
    @Mock
    Logger logger;
    @Mock
    ResponseDto dto;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);
    }

    @Test(expected = Exception.class)
    public void getSaasSubscriptionTest() throws IOException {
        Map<String,Object> map=new HashMap<>();
        map.put("saas_registry_url","test");
        map.put("url","test");
        map.put("clientid","test");
        map.put("clientsecret","test");
        map.put("saas-registry","test");
        Mockito.when(env.findCredentialsByLabel(any(String.class))).thenReturn(cfCredentials);
        when(cfCredentials.getMap()).thenReturn(map);
        String tken="test";
        when(xsuaa.getAccessToken(any(),any(),any())).thenReturn(tken);
        when(httpService.get(any(),any(),any())).thenReturn(dto);
        when(dto.getContent()).thenReturn("dto");
        service.getSaasSubscription();
    }
}
