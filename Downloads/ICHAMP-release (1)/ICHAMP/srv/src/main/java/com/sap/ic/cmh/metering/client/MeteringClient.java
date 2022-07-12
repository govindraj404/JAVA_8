package com.sap.ic.cmh.metering.client;

import com.google.gson.Gson;
import com.sap.ic.cmh.metering.model.*;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.apache.http.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.springframework.context.annotation.Profile;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

@Component
@Profile("cloud")
public class MeteringClient {

    @Autowired
    PlatformUtil platformUtil;

    Gson gson = new Gson();


    private static final Logger LOG = LoggerFactory.getLogger(MeteringClient.class);

    public boolean meteringServiceClient(Long count, Subscription subscriptionForCurrentTenant) {
        RestTemplate restTemplate = new RestTemplateBuilder().build();
        try {
            Map<String, Object> meteringVCap = platformUtil.getServiceCredentials(Constants.METERING_SERVICE);
            String region = (String) meteringVCap.get(Constants.REGION);
            String meteringUrlBasePath = (String) meteringVCap.get(Constants.METERING_URL);
            HttpHeaders headers = getAuthHeader(meteringVCap);

            UsageDocument usageDoc = getUsageDocumentRequest(count, region, subscriptionForCurrentTenant);
            List<UsageDocument> usageList = new ArrayList<>();
            usageList.add(usageDoc);
            UsageDocumentsData usageDocuments = new UsageDocumentsData();
            usageDocuments.setUsage(usageList);

            HttpEntity<String> request = new HttpEntity<>(gson.toJson(usageDocuments), headers);
            String meteringUrl = meteringUrlBasePath + "/usage/v3/usage/documents";
            ResponseEntity<String> responseString = restTemplate.exchange(meteringUrl, HttpMethod.PUT, request, String.class);

            if (responseString.getStatusCode().is2xxSuccessful()) {
                LOG.info(responseString.getStatusCode() + " Success");
                return true;
            }
            return false;
        } catch (ParseException e) {
            LOG.error("Error while hitting metering API");
            return false;
        }
    }

    public HttpHeaders getAuthHeader(Map<String, Object> meteringVCap) {
        String clientId = (String) meteringVCap.get(Constants.UAA_CLIENT_ID);
        String clientSecret = (String) meteringVCap.get(Constants.UAA_CLIENT_SECRET);
        String tokenUrl = (String) meteringVCap.get(Constants.TOKEN_URL);

        RestTemplate restTemplate = new RestTemplateBuilder().basicAuthentication(clientId, clientSecret).build();
        ResponseEntity<OAuthToken> response = restTemplate.getForEntity(tokenUrl + "/oauth/token?grant_type=client_credentials", OAuthToken.class);
        HttpHeaders headers = new HttpHeaders();
        if (response.getBody() != null) {
            headers.add("Authorization", "Bearer " + response.getBody().getAccessToken());
            headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
        }
        return headers;
    }

    public UsageDocument getUsageDocumentRequest(
            long totalRecords, String region, Subscription subscriptionForCurrentTenant) {
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constants.METERING_TIMESTAMP);

        List<Measure> measures = new ArrayList<>();
        Measure measure = new Measure();
        measure.setId(Constants.MEASURE_ID);
        measure.setValue(totalRecords);
        measures.add(measure);

        Consumer consumer = new Consumer();
        consumer.setEnvironment(Constants.CONSUMER_ENVIRONMENT);
        consumer.setRegion(region);
        consumer.setSubAccount(subscriptionForCurrentTenant.getSubaccountId());

        Service service = new Service();
        service.setId(Constants.SERVICE_ID);
        service.setPlan(Constants.SERVICE_PLAN);

        UsageDocument usageDocument = new UsageDocument();
        usageDocument.setTimestamp(simpleDateFormat.format(new Date()));
        usageDocument.setConsumer(consumer);
        usageDocument.setService(service);
        usageDocument.setMeasures(measures);

        return usageDocument;
    }
}
