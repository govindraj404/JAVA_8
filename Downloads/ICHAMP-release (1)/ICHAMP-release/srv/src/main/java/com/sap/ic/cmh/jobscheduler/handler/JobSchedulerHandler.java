package com.sap.ic.cmh.jobscheduler.handler;

import com.sap.ic.cmh.jobscheduler.service.JobSchedulerService;
import com.sap.ic.cmh.jobscheduler.utils.JobSchedulerUtils;
import com.sap.ic.cmh.utils.LoggerHelper;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import java.util.HashMap;
import java.util.Map;

@Component
public class JobSchedulerHandler {

    private static final Logger logger = LoggerFactory.getLogger(JobSchedulerHandler.class);
    private static final String JOB_SCHEDULER_HANDLER = "JobSchedulerHandler";
    private static final String CREATE_DEFAULT_JOBS = "createDefaultJobs";

    @Autowired
    JobSchedulerUtils jobSchedulerUtils;
    @Autowired
    JobSchedulerService jobSchedulerService;

    /*
     * Method to automatically create all the default jobs
     */
    public void createDefaultJobs(String subscribedSubdomain) {
    	LoggerHelper.logMethodEntry(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
        String jobSchedulerUrl = jobSchedulerUtils.getJobSchedulerServiceURL() + "/scheduler/jobs";

        JSONArray defaultJobs = jobSchedulerService.getDefaultJobs();
        for(int i=0; i < defaultJobs.length(); i++) {
            JSONObject jobObj = defaultJobs.getJSONObject(i);
            createJob(jobObj, subscribedSubdomain, getHttpHeaders(subscribedSubdomain), jobSchedulerUrl);
        }
        LoggerHelper.logMethodExit(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
    }

    /*
     * Method to automatically create all the default jobs
     */
    public Map<String, Object> createNewJob(String subscribedSubdomain, JSONObject jobDetails) {
        String jobSchedulerUrl = jobSchedulerUtils.getJobSchedulerServiceURL() + "/scheduler/jobs";

        return createJob(jobDetails, subscribedSubdomain, getHttpHeaders(subscribedSubdomain), jobSchedulerUrl);
    }

    private Map<String, Object> createJob(JSONObject jobObj, String subscribedSubdomain, HttpHeaders headers, String jobSchedulerUrl) {
    	LoggerHelper.logMethodEntry(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
        RestTemplate restTemplate = new RestTemplate();
        Map<String, Object> response = new HashMap<>();
        String status = "Error while creating the job";
        subscribedSubdomain = subscribedSubdomain.replaceAll("[^a-zA-Z]", " ");
        jobObj.put("name", subscribedSubdomain + " " + jobObj.getString("name"));
        HttpEntity<String> entity = new HttpEntity<>(jobObj.toString(), headers);
        response.put("payload", jobObj.toMap());
        if(jobExists(headers, jobSchedulerUrl, jobObj.getString("name"))) {
            response.put("status", "FAILED");
            response.put("statusCode", 409);
            response.put("response", "Job already exists");
            return response;
        }
        ResponseEntity<String> responseString = restTemplate.exchange(jobSchedulerUrl, HttpMethod.POST, entity, String.class);

        if (responseString.getStatusCode().is2xxSuccessful()) {
            status = "Job successfully created";
        }

        response.put("status", responseString.getStatusCode());
        response.put("statusCode", responseString.getStatusCodeValue());
        response.put("response", status);
        LoggerHelper.logMethodExit(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
        return response;
    }

    private HttpHeaders getHttpHeaders(String subscribedSubdomain) {
    	LoggerHelper.logMethodEntry(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
        HttpHeaders headers = new HttpHeaders();
        String accessToken = jobSchedulerUtils.getJobSchedulerJWT(subscribedSubdomain).getAccessToken();
        headers.add("Authorization", "Bearer " + accessToken);
        headers.add("Content-Type", "application/json");
        LoggerHelper.logMethodExit(logger, JOB_SCHEDULER_HANDLER, CREATE_DEFAULT_JOBS);
        return headers;
    }


    private boolean jobExists(HttpHeaders headers, String jobSchedulerUrl, String jobName) {
    	LoggerHelper.logMethodEntry(logger, JOB_SCHEDULER_HANDLER, "createJob");
        RestTemplate restTemplate = new RestTemplate();
        HttpEntity<String> entity = new HttpEntity<>(headers);
        ResponseEntity<String> responseString = restTemplate.exchange(jobSchedulerUrl, HttpMethod.GET, entity, String.class);
        JSONObject jsonObj = new JSONObject(responseString.getBody());
        JSONArray jobsArray = jsonObj.getJSONArray("results");
        for(int i=0;i<jobsArray.length();i++) {
            JSONObject jobObject = jobsArray.getJSONObject(i);
            if((jobObject.getString("name")).equals(jobName))
            {
                return true;
            }
        }
        LoggerHelper.logMethodExit(logger, JOB_SCHEDULER_HANDLER, "createJob");
        return false;
    }
}
