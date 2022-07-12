package com.sap.ic.cmh.jobscheduler.handler;

import com.sap.ic.cmh.jobscheduler.service.JobSchedulerService;
import com.sap.ic.cmh.jobscheduler.utils.JobSchedulerUtils;
import com.sap.ic.cmh.model.OAuthToken;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import java.util.HashMap;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class JobSchedulerHandlerTest {

    @InjectMocks
    @Autowired
    JobSchedulerHandler jobSchedulerHandler;
    @Mock
    JobSchedulerUtils jobSchedulerUtils;
    @Mock
    JobSchedulerService jobSchedulerService;
    @Mock
    OAuthToken oAuthToken;
    @Mock
    RestTemplate restTemplate;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        oAuthToken.setAccessToken("test");
        oAuthToken.setTokenType("test");
        oAuthToken.setExpiresIn("test");
    }

    @Test
    public void testCreateDefaultJobs() {
        when(jobSchedulerUtils.getJobSchedulerJWT("test")).thenReturn(oAuthToken);
        when(oAuthToken.getAccessToken()).thenReturn("access_token");
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn("");
        JSONArray defaultJobs = new JSONArray();
        Map<String, String> nameMap = new HashMap<>();
        nameMap.put("name", "value");
        defaultJobs.put(nameMap);
        when(jobSchedulerService.getDefaultJobs()).thenReturn(defaultJobs);
        String jobSchedulerUrl = jobSchedulerUtils.getJobSchedulerServiceURL() + "/scheduler/jobs";
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn(jobSchedulerUrl);

        try (MockedConstruction<RestTemplate> mocked = Mockito.mockConstruction(RestTemplate.class,
                (mock, context) -> {
                    ResponseEntity<String> responseEntity = new ResponseEntity<String>("{'results': [{'name':'test'}]}", HttpStatus.ACCEPTED);
                    when(mock.exchange(
                                    Matchers.anyString(),
                                    any(HttpMethod.class),
                                    Matchers.<HttpEntity<?>> any(),
                                    Matchers.<Class<String>> any()
                            )
                    ).thenReturn(responseEntity);
                })) {

            jobSchedulerHandler.createDefaultJobs("test");
        }
    }

    @Test
    public void testCreateDefaultJobsWhereJobExists() {
        when(jobSchedulerUtils.getJobSchedulerJWT("test")).thenReturn(oAuthToken);
        when(oAuthToken.getAccessToken()).thenReturn("access_token");
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn("");
        JSONArray defaultJobs = new JSONArray();
        Map<String, String> nameMap = new HashMap<>();
        nameMap.put("name", "value");
        defaultJobs.put(nameMap);
        when(jobSchedulerService.getDefaultJobs()).thenReturn(defaultJobs);
        String jobSchedulerUrl = jobSchedulerUtils.getJobSchedulerServiceURL() + "/scheduler/jobs";
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn(jobSchedulerUrl);

        try (MockedConstruction<RestTemplate> mocked = Mockito.mockConstruction(RestTemplate.class,
                (mock, context) -> {
                    ResponseEntity<String> responseEntity = new ResponseEntity<String>("{'results': [{'name':'test value'}]}", HttpStatus.ACCEPTED);
                    when(mock.exchange(
                                    Matchers.anyString(),
                                    any(HttpMethod.class),
                                    Matchers.<HttpEntity<?>> any(),
                                    Matchers.<Class<String>> any()
                            )
                    ).thenReturn(responseEntity);
                })) {

            jobSchedulerHandler.createDefaultJobs("test");
        }
    }

    @Test(expected = Exception.class)
    public void testCreateNewJob() {
        JSONObject jobDetails = new JSONObject();
        when(jobSchedulerUtils.getJobSchedulerJWT("test")).thenReturn(oAuthToken);
        when(oAuthToken.getAccessToken()).thenReturn("access_token");
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn("");
        String jobSchedulerUrl = jobSchedulerUtils.getJobSchedulerServiceURL() + "/scheduler/jobs";
        when(jobSchedulerUtils.getJobSchedulerServiceURL()).thenReturn(jobSchedulerUrl);

        jobDetails.put("name", "test");
        jobSchedulerHandler.createNewJob("test", jobDetails);
    }
}
