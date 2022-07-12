package com.sap.ic.cmh.jobscheduler.controller;

import com.sap.ic.cmh.jobscheduler.handler.JobSchedulerHandler;
import com.sap.ic.cmh.returnpo.service.ReturnPurchaseOrderService;
import com.sap.ic.cmh.supplierissueprocess.service.EightDService;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/job")
public class JobSchedulerController {

    public static final Logger logger = LoggerFactory.getLogger(JobSchedulerController.class);

    @Autowired
    EightDService eightDService;
    @Autowired
    ReturnPurchaseOrderService returnPurchaseOrderService;
    @Autowired
    TenantSubscriptionService tenantSubscriptionService;
    @Autowired
    JobSchedulerHandler jobSchedulerHandler;

    @GetMapping("/cmh/{jobName}")
    public void callJob(@PathVariable("jobName") String jobName) {
        LoggerHelper.logMethodEntry(logger, "JobSchedulerController", "callJob");
        // Call job based on job name
		if(jobName.equalsIgnoreCase(Constants.JOB_SCHEDULER_SUPPLIER_ISSUE_PROCESS_API)) { 
            // Supplier Issue Process Job              
            try {
                logger.info("*****************SUPPLIER ISSUE PROCESS JOB TRIGGERED*********************");
                eightDService.mapEightDStatus();
            }catch (Exception e) {
                logger.error("Supplier Issue Process Job exception thrown", e);
            }
        }
		LoggerHelper.logMethodExit(logger, "JobSchedulerController", "callJob");
    }

    @PostMapping("/cmh/createJob")
    public Map<String, Map<String, Object>> createNewJobForAllTenants(@RequestBody Map<String, Object> jobDetailsMap) {
        Map<String, Map<String, Object>> jobCreationStatus = new HashMap<>();
        List<Subscription> subscriptionList = tenantSubscriptionService.getSaasSubscription().getSubscriptions();
        subscriptionList.forEach(subscription -> {
            JSONObject jobDetails = new JSONObject(jobDetailsMap);
            Map<String, Object> jobCreationResponse = jobSchedulerHandler.createNewJob(subscription.getSubdomain(), jobDetails);
            jobCreationStatus.put(subscription.getConsumerTenantId(), jobCreationResponse);
        });

        return jobCreationStatus;
    }
}
