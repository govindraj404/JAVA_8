package com.sap.ic.cmh.jobscheduler.utils;

import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class JobSchedulerUtils {

    public static final Logger logger = LoggerFactory.getLogger(JobSchedulerUtils.class);

    @Autowired
    PlatformUtil platformUtil;

    private static final String JOB_SCHEDULER = "jobscheduler";
    private static final String JOB_SCHEDULER_SERVICE_URL = "url";
    private static final String JOB_SCHEDULER_SERVICE_UAA = "uaa";
    private static final String CLIENT_ID = "clientid";
    private static final String CLIENT_SECRET = "clientsecret";
    private static final String UAA_DOMAIN = "uaadomain";

    /*
     * Method to bring oauth token for a tenant of job scheduler
     */
    public OAuthToken getJobSchedulerJWT(String subdomain){
        RestTemplate restTemplate;
        JSONObject jobSchedulerUAADetails = platformUtil.getCredentials(JOB_SCHEDULER).getJSONObject(JOB_SCHEDULER_SERVICE_UAA);
        String clientId = jobSchedulerUAADetails.getString(CLIENT_ID);
        String clientSecret = jobSchedulerUAADetails.getString(CLIENT_SECRET);
        String uaaDomain = jobSchedulerUAADetails.getString(UAA_DOMAIN);
        String tokenUrl = "https://" + subdomain + "." + uaaDomain + "/oauth/token?grant_type=client_credentials";
        restTemplate = new RestTemplateBuilder().basicAuthentication(clientId,clientSecret).build();
        ResponseEntity<OAuthToken> response = restTemplate.getForEntity(tokenUrl, OAuthToken.class);
        return response.getBody();
    }

    /*
     * Method to bring service URL of job scheduler service
     */
    public String getJobSchedulerServiceURL()
    {
        return platformUtil.getCredentials(JOB_SCHEDULER).getString(JOB_SCHEDULER_SERVICE_URL);
    }
}
