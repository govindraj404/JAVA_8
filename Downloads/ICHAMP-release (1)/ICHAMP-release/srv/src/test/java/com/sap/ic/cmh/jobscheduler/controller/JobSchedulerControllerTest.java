package com.sap.ic.cmh.jobscheduler.controller;

import com.sap.ic.cmh.jobscheduler.handler.JobSchedulerHandler;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.tenancy.model.SaasSubscription;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.Mockito.*;

public class JobSchedulerControllerTest {

    @InjectMocks
    @Autowired
    JobSchedulerController jobSchedulerController;
    @Mock
    TenantSubscriptionService tenantSubscriptionService;
    @Mock
    SaasSubscription saasSubscription;
    @Mock
    EightDService eightDService;
    @Mock
    Subscription subscription;
    @Mock
    JobSchedulerHandler jobSchedulerHandler;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        subscriptionList.add(subscription);
    }

    Map<String, Object> jobDetailsMap = new HashMap<>();
    List<Subscription> subscriptionList = new ArrayList<>();
    Map<String, Object> jobCreationResponse = new HashMap<>();

    @Test
    public void testCallJobSIPStatus() {
        doNothing().when(eightDService).mapEightDStatus();
        jobSchedulerController.callJob("sipstatus");
    }

    @Test
    public void testCallJobOtherStatus() {
        jobSchedulerController.callJob("Othersipstatus");
    }

    @Test
    public void testCallJobSIPStatusException() {
        doThrow(NullPointerException.class).when(eightDService).mapEightDStatus();
        jobSchedulerController.callJob("sipstatus");
    }

    @Test
    public void testCreateNewJobForAllTenants() {
        when(tenantSubscriptionService.getSaasSubscription()).thenReturn(saasSubscription);
        when(saasSubscription.getSubscriptions()).thenReturn(subscriptionList);
        when(jobSchedulerHandler.createNewJob(any(), any())).thenReturn(jobCreationResponse);
        jobSchedulerController.createNewJobForAllTenants(jobDetailsMap);
    }
}
