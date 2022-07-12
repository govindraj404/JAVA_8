package com.sap.ic.cmh.tenancy.handler;

import com.google.gson.JsonObject;
import com.sap.cds.services.mt.MtGetDependenciesEventContext;
import com.sap.cds.services.mt.MtAsyncSubscribeEventContext;
import com.sap.cloud.mt.subscription.json.SubscriptionPayload;
import com.sap.cloud.mt.subscription.json.SubscriptionPayloadAddInfo;
import com.sap.cloud.sdk.cloudplatform.ScpCfCloudPlatform;
import com.sap.ic.cmh.jobscheduler.handler.JobSchedulerHandler;
import io.vavr.control.Option;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.OngoingStubbing;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
//PrepareForTest(SubscriptionHandler.class,EventHandler.class)
public class SubscriptionHandlerTest {
    @InjectMocks
    SubscriptionHandler subscriptionHandler;
    @Mock
    JobSchedulerHandler jobSchedulerHandler;
    @Mock
    ScpCfCloudPlatform cfPlatformApi;
    @Mock
    JsonObject jsonObject;
    @Mock
    MtGetDependenciesEventContext context;
    @Mock
    MtAsyncSubscribeEventContext eventContext;
    @Mock
    SubscriptionPayload payload;
    @Mock
    SubscriptionPayloadAddInfo info;
    @Mock
    ScpCfCloudPlatform scpCfCloudPlatform;

    @Mock
    JsonObject connectivityCredentials;
    @Mock
    JsonObject destinationCredentials;
    @Mock
    JsonObject portalCredential;
    @Mock
    JsonObject jobSchedulerCredentials;
    @Mock
    JsonObject pdmCredentials;
    @Mock
    JsonObject drmCredentials;
    private static final String XSAPPNAME = "xsappname";
    private OngoingStubbing<String> destXSAppName;
    private OngoingStubbing<String> connectivityXSAppName;
    private OngoingStubbing<String> portalXSAppName;
    private OngoingStubbing<String> jobSchedulerXSAppName;
    private OngoingStubbing<String> pdmXSAppname;
    private OngoingStubbing<String> drmXSAppname;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test(expected = Exception.class)
    public void testGetVcapServiceCredentials() {
        String env1 = "VCAP_SERVICES";
        Option<String> test = Option.of(env1);
        cfPlatformApi = (ScpCfCloudPlatform) when(ScpCfCloudPlatform.getInstanceOrThrow()).thenReturn(cfPlatformApi);
        when(cfPlatformApi.getEnvironmentVariable(any())).thenReturn(test);
        when(cfPlatformApi.getConnectivityServiceCredentials()).thenReturn(jsonObject);
        when(cfPlatformApi.getServiceCredentials("portal")).thenReturn(jsonObject);
        when(cfPlatformApi.getServiceCredentials("jobscheduler")).thenReturn(jsonObject);
        when(cfPlatformApi.getServiceCredentials("personal-data-manager-service")).thenReturn(jsonObject);
        when(this.cfPlatformApi.getServiceCredentials("retention-manager")).thenReturn(jsonObject);
        when(this.cfPlatformApi.getConnectivityServiceCredentials()).thenReturn(connectivityCredentials);
        when(this.cfPlatformApi.getDestinationServiceCredentials()).thenReturn(destinationCredentials);
        when(this.cfPlatformApi.getServiceCredentials("portal")).thenReturn(portalCredential);
        when(this.cfPlatformApi.getServiceCredentials("jobscheduler")).thenReturn(jobSchedulerCredentials);
        when(this.cfPlatformApi.getServiceCredentials("personal-data-manager-service")).thenReturn(pdmCredentials);
        when(this.cfPlatformApi.getServiceCredentials("retention-manager")).thenReturn(drmCredentials);
        connectivityXSAppName = when(connectivityCredentials.get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(connectivityXSAppName));
        destXSAppName = when(destinationCredentials.get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(destXSAppName));
        portalXSAppName = when(portalCredential.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(portalXSAppName));
        jobSchedulerXSAppName = when(jobSchedulerCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(jobSchedulerXSAppName));
        pdmXSAppname = when(pdmCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(pdmXSAppname));
        drmXSAppname = when(drmCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString()).thenReturn(String.valueOf(drmXSAppname));
        subscriptionHandler.getVcapServiceCredentials();
    }

    @Test
    public void onGetDependenciesTest() {
        subscriptionHandler.onGetDependencies(context);
    }
    @Test
    public void afterSubscriptionTest() {
        payload.additionalInformation=info;
        info.tokenurl="test";
        when(eventContext.getSubscriptionPayload()).thenReturn(payload);
        subscriptionHandler.afterAsyncSubscription(eventContext);
    }
}