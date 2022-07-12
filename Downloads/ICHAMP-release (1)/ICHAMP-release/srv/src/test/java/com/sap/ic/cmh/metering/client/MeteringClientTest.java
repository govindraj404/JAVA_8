package com.sap.ic.cmh.metering.client;

import com.google.gson.Gson;
import com.sap.ic.cmh.metering.model.*;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.apache.http.ParseException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.text.SimpleDateFormat;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class MeteringClientTest {
    @InjectMocks
    MeteringClient client;
    @Mock
    PlatformUtil platformUtil;
    @Mock
    RestTemplateBuilder builder;
    @Mock
    RestTemplate restTemplate;
    @Mock
    SimpleDateFormat simpleDateFormat;
    @Mock
    List<Measure> measuresList;
    @Mock
    Measure measure;
    @Mock
    Consumer consumer;
    @Mock
    Service service;
    @Mock
    UsageDocument usageDocument;
    @Mock
    Gson gson ;
    @Mock
    Subscription subscriptionForCurrentTenant;
    Map<String, Object> meteringVCap;
    long totalRecords;
    Long count;
    String region;
    String tenantId;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Mock
    OAuthToken oAuthToken;

    @Test(expected = Exception.class)
    public void meteringServiceClientTest() {
        Map<String, Object> map = new HashMap<>();
        map.put("region", "region");
        map.put("metering_url", "/test/Username:c53103922?password:test123/");
        map.put("clientid", "1231231231242134");
        map.put("clientsecret", "1231231231242134");
        map.put("token_url", "/http:localhost:8080/");
        ResponseEntity r = new ResponseEntity(HttpStatus.ACCEPTED);
        Mockito.when(platformUtil.getServiceCredentials(any(String.class))).thenReturn(map);
        Mockito.when(restTemplate.getForEntity("http://localhost:8080/", OAuthToken.class)).thenReturn(new ResponseEntity(oAuthToken, HttpStatus.OK));
        client.meteringServiceClient(10L,subscriptionForCurrentTenant);
    }
@Mock
   Subscription subscription;

    @Test
    public void testGetUsageDocumentRequest() {
        simpleDateFormat = new SimpleDateFormat(Constants.METERING_TIMESTAMP);
       usageDocument=new UsageDocument();
        measuresList = new ArrayList<>();
        measure.setId(Constants.MEASURE_ID);
        measure.setValue(totalRecords);
        measuresList.add(measure);
        consumer.setEnvironment(Constants.CONSUMER_ENVIRONMENT);
        consumer.setRegion(region);
        consumer.setSubAccount(tenantId);
        service.setId(Constants.SERVICE_ID);
        service.setPlan(Constants.SERVICE_PLAN);
        usageDocument.setTimestamp(simpleDateFormat.format(new Date()));
        usageDocument.setConsumer(consumer);
        usageDocument.setId("10");
        usageDocument.getId();
        usageDocument.setService(service);
        usageDocument.setMeasures(measuresList);
        client.getUsageDocumentRequest(10, consumer.getRegion(), subscription);
    }

    @Test(expected = Exception.class)
    public void testMeteringServiceClientException(){
         restTemplate = new RestTemplateBuilder().build();
         gson= new Gson();
        try {
            Map<String, Object> meteringVCap = platformUtil.getServiceCredentials(Constants.METERING_SERVICE);
            String region = (String) meteringVCap.get(Constants.REGION);
            String meteringUrlBasePath = (String) meteringVCap.get(Constants.METERING_URL);
            HttpHeaders headers = client.getAuthHeader(meteringVCap);
            String tenantId = (String) meteringVCap.get(Constants.TENANT_ID);
            UsageDocument usageDoc = client.getUsageDocumentRequest(count, region, subscriptionForCurrentTenant);
            List<UsageDocument> usageList = new ArrayList<>();
            usageList.add(usageDoc);
            UsageDocumentsData usageDocuments = new UsageDocumentsData();
            usageDocuments.setUsage(usageList);
            HttpEntity<String> request = new HttpEntity<>(gson.toJson(usageDocuments), headers);
            String meteringUrl = meteringUrlBasePath + "/usage/v2/usage/documents";
            ResponseEntity<String> responseString = restTemplate.exchange(meteringUrl, HttpMethod.PUT, request, String.class);
            client.meteringServiceClient(10L,subscription);
        }
        catch (ParseException  e)
        {
        }
    }
}

