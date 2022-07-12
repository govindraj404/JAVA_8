package com.sap.ic.cmh.tenancy.handler;

import static com.sap.cloud.sdk.cloudplatform.ScpCfCloudPlatform.getInstanceOrThrow;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;
import com.google.gson.JsonObject;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.mt.MtGetDependenciesEventContext;
import com.sap.cds.services.mt.MtAsyncSubscribeEventContext;
import com.sap.cds.services.mt.MtSubscriptionService;
import com.sap.cloud.mt.subscription.json.ApplicationDependency;
import com.sap.cloud.sdk.cloudplatform.ScpCfCloudPlatform;
import com.sap.ic.cmh.jobscheduler.handler.JobSchedulerHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/** Handler that implements subscription logic */
@Component
@Profile("cloud")
@ServiceName(MtSubscriptionService.DEFAULT_NAME)
class SubscriptionHandler implements EventHandler {

    public static final Logger logger = LoggerFactory.getLogger(SubscriptionHandler.class);

    @Autowired
    JobSchedulerHandler jobSchedulerHandler;

    public SubscriptionHandler() {
        this.cfPlatformApi = getInstanceOrThrow();
    }

    private static final String XSAPPNAME = "xsappname";
    private final ScpCfCloudPlatform cfPlatformApi;
    private String destXSAppName;
    private String connectivityXSAppName;
    private String portalXSAppName;
    private String jobSchedulerXSAppName;
    private String pdmXSAppname;
    private String drmXSAppname;

    @PostConstruct
    public void getVcapServiceCredentials() {
        JsonObject connectivityCredentials = this.cfPlatformApi.getConnectivityServiceCredentials();
        JsonObject destinationCredentials = this.cfPlatformApi.getDestinationServiceCredentials();
        JsonObject portalCredentials = this.cfPlatformApi.getServiceCredentials("portal");
        JsonObject jobSchedulerCredentials = this.cfPlatformApi.getServiceCredentials("jobscheduler");
        JsonObject pdmCredentials = this.cfPlatformApi.getServiceCredentials("personal-data-manager-service");
        JsonObject drmCredentials = this.cfPlatformApi.getServiceCredentials("retention-manager");

        connectivityXSAppName = connectivityCredentials.get(XSAPPNAME).getAsString();
        destXSAppName = destinationCredentials.get(XSAPPNAME).getAsString();
        portalXSAppName = portalCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString();
        jobSchedulerXSAppName = jobSchedulerCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString();
        pdmXSAppname = pdmCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString();
        drmXSAppname = drmCredentials.get("uaa").getAsJsonObject().get(XSAPPNAME).getAsString();
    }

    @On(event = MtSubscriptionService.EVENT_GET_DEPENDENCIES)
    public void onGetDependencies(MtGetDependenciesEventContext context) {

        ApplicationDependency destDependency = new ApplicationDependency();
        destDependency.xsappname = destXSAppName;

        ApplicationDependency connectivityDependency = new ApplicationDependency();
        connectivityDependency.xsappname = connectivityXSAppName;

        ApplicationDependency portalDependency = new ApplicationDependency();
        portalDependency.xsappname = portalXSAppName;

        ApplicationDependency jobSchedulerDependency = new ApplicationDependency();
        jobSchedulerDependency.xsappname = jobSchedulerXSAppName;

        ApplicationDependency pdmDependency = new ApplicationDependency();
        pdmDependency.xsappname = pdmXSAppname;

        ApplicationDependency drmDependency = new ApplicationDependency();
        drmDependency.xsappname = drmXSAppname;

        List<ApplicationDependency> dependencies = new ArrayList<>();
        dependencies.add(destDependency);
        dependencies.add(connectivityDependency);
        dependencies.add(portalDependency);
        dependencies.add(jobSchedulerDependency);
        dependencies.add(pdmDependency);
        dependencies.add(drmDependency);
        context.setResult(dependencies);
    }

    @After(event = MtSubscriptionService.EVENT_ASYNC_SUBSCRIBE)
    public void afterAsyncSubscription(MtAsyncSubscribeEventContext context) {
        logger.info("***************EVENT ASYNC SUBSCRIBE TRIGGERED**********************");
        String subscribedSubdomain = context.getSubscriptionPayload().subscribedSubdomain;
        String tokenUrl = context.getSubscriptionPayload().additionalInformation.tokenurl;
        logger.info("Token URL: " + tokenUrl);
        jobSchedulerHandler.createDefaultJobs(subscribedSubdomain);
    }
}