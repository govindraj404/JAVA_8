package com.sap.ic.cmh.jobscheduler.service;

import com.sap.ic.cmh.jobscheduler.utils.JobSchedulerUtils;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class JobSchedulerService {

    @Autowired
    JobSchedulerUtils jobSchedulerUtils;
    @Autowired
    PlatformUtil platformUtil;

    /*
     * Method to get default job delivered to customer from user provided service
     */
    public JSONArray getDefaultJobs() {
        JSONObject jobUserProvidedServiceCreds = platformUtil.getUserProvidedInstanceCredentials("cmh-default-jobs");
        JSONArray jobsArray = new JSONArray();
        if(jobUserProvidedServiceCreds != null && !jobUserProvidedServiceCreds.isEmpty()) {
            jobsArray = jobUserProvidedServiceCreds.getJSONArray("jobs");
        }
        return jobsArray;
    }
}
