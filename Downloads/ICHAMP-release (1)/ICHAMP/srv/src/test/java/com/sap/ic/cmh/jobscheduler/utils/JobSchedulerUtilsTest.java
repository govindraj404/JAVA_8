package com.sap.ic.cmh.jobscheduler.utils;

import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

public class JobSchedulerUtilsTest {

    @InjectMocks
    @Autowired
    JobSchedulerUtils jobSchedulerUtils;
    @Mock
    PlatformUtil platformUtil;
    @Mock
    ResponseEntity<OAuthToken> response;
    @Mock
    RestTemplate restTemplate;
    String subdomain;

    @Mock
    RestTemplateBuilder restTemplateBuilder;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetJobSchedulerJWT() {
        JSONObject jsonObject = new JSONObject();
        JSONObject uaaObject = new JSONObject();
        uaaObject.put("clientid", "id");
        uaaObject.put("clientsecret", "secret");
        uaaObject.put("uaadomain", "uaadomain");
        jsonObject.put("uaa", uaaObject);
        jsonObject.put("subdomain", "value");
        jsonObject.put("uaadomain", "uaadomain");

        Mockito.when(platformUtil.getCredentials("jobscheduler")).thenReturn(jsonObject);


        try (MockedConstruction<RestTemplate> mocked = Mockito.mockConstruction(RestTemplate.class,
                (mock, context) -> {
                    String json = "{\n"
                            + "\"specversion\" : \"1.0\",\n"
                            + "\"type\" : \"sap.nw.ee.quality_notification.Created.v1\",\n"
                            + "\"source\":\"/default/sap.nw.ee/000000000312413320\",\n"
                            + "\"id\" : \"QN_CREATED20210829182212.2832790 \",\n"
                            + "\"time\" : \"2021-08-29T18:22:12.283Z\",\n"
                            + "\"data\" :\n"
                            + "{\"QMEL\":[{\"MANDT\":\"100\",\"QMNUM\":\"000200012804\",\"MATNR\":\"BATTERY\",\"QMART\":\"F2\",\"MGEIN\":\"ST\",\"EKORG\":\"0001\",\"LIFNUM\":\"SUPPLIER\",\"ERNAM\":\"I548835\",\"MAWERK\":\"1000\",\"RKMNG\":1.000,\"MGFRD\":1.000,\"EBELN\":\"4500000554\",\"EBELP\":\"00010\",\"QMFE\":[{\"FEGRP\":\"QM-E\",\"FECOD\":\"1\",\"FENUM\":\"0001\",\"JEST\":[{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0068\",\"INACT\":\"\",\"CHGNR\":\"001\"},{\"OBJNR\":\"QM000200012804\",\"STAT\":\"I0161\",\"INACT\":\"\",\"CHGNR\":\"001\"}]}]}]}\n"
                            + "}";
                    ResponseEntity<Object> responseEntity = new ResponseEntity<Object>(new OAuthToken(), HttpStatus.ACCEPTED);
                    when(mock.getForEntity(Matchers.anyString(), Matchers.any())).thenReturn(responseEntity);
                })) {

            assertNotNull(jobSchedulerUtils.getJobSchedulerJWT("subdomain"));
        }

    }

    @Test
    public void testGetJobSchedulerServiceURL() {
        JSONObject jsonObject = Mockito.mock(JSONObject.class);
        jsonObject.put("url", "value");
        Mockito.when(platformUtil.getCredentials("jobscheduler")).thenReturn(jsonObject);
        Mockito.when(jsonObject.getString("url")).thenReturn("value");
        assertEquals("value", jobSchedulerUtils.getJobSchedulerServiceURL());
    }
}
