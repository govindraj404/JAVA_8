package com.sap.ic.cmh.jobscheduler.service;

import com.sap.ic.cmh.jobscheduler.utils.JobSchedulerUtils;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.Assert.assertEquals;

public class JobSchedulerServiceTest {

    @InjectMocks
    @Autowired
    JobSchedulerService jobSchedulerService;
    @Mock
    JobSchedulerUtils jobSchedulerUtils;
    @Mock
    PlatformUtil platformUtil;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetDefaultJobs() {
        JSONArray jobsArray = new JSONArray();
        JSONObject jobUserProvidedServiceCreds = new JSONObject();
        jobUserProvidedServiceCreds.put("jobs", jobsArray);
        Mockito.when(platformUtil.getUserProvidedInstanceCredentials("cmh-default-jobs")).thenReturn(jobUserProvidedServiceCreds);
        assertEquals(jobsArray, jobSchedulerService.getDefaultJobs());
    }

    @Test
    public void testGetDefaultJobsNull() {
        Mockito.when(platformUtil.getUserProvidedInstanceCredentials("cmh-default-jobs")).thenReturn(null);
        jobSchedulerService.getDefaultJobs();
    }
}
