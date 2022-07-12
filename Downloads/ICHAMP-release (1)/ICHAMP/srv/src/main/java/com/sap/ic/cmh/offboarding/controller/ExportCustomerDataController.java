package com.sap.ic.cmh.offboarding.controller;

import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses;
import com.sap.cds.Struct;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cds.services.runtime.RequestContextRunner;
import com.sap.cloud.security.xsuaa.token.SpringSecurityContext;
import com.sap.cloud.security.xsuaa.token.Token;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.objectstore.service.ObjectStoreService;
import com.sap.ic.cmh.offboarding.persistency.CustomerDataExportStatusDao;
import com.sap.ic.cmh.offboarding.service.ExportCustomerDataService;
import com.sap.ic.cmh.utils.GenericUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;

@RestController
@RequestMapping("/exportdatabase")
public class ExportCustomerDataController {

    @Autowired
    ExportCustomerDataService exportCustomerDataService;
    @Autowired
    CdsRuntime cdsRuntime;
    @Autowired
    CustomerDataExportStatusDao customerDataExportStatusDao;
    @Autowired
    ObjectStoreService objectStoreService;
    @Autowired
    AuditLogHelper auditLogHelper;

    private static final Logger logger = LoggerFactory.getLogger(ExportCustomerDataController.class);
    private static final String UNABLE_TO_VERIFY_USER = "Unable to verify the user. Please use the Password Credentials to generate the token.";
    private static final String STATUS = "Status";
    Map<String, Object> auditLogData = new HashMap<>();
    List<Map<String, Object>> auditLogDataList = new ArrayList<>();

    @GetMapping("/tenant/{tenantId}")
    public ResponseEntity<String> exportData(@PathVariable("tenantId") String tenantId, @RequestParam("subdomain") String subdomain) {
        logger.info("EXPORT TRIGGERED FOR TENANT");
        Token token = SpringSecurityContext.getToken();

        return getModifiedUser(tenantId, token).run(context -> {
            if (token.getEmail() != null) {
                JSONObject tenantDBContainers = exportCustomerDataService.getTenantDBContainers(tenantId);
                JSONArray tenantDBContainerArray = tenantDBContainers.getJSONArray("items");

                auditLogData.put(STATUS, "Job Triggered");
                auditData(tenantId, token, auditLogData);

                if (!tenantDBContainerArray.isEmpty()) {
                    CustomerDataExportStatuses customerDataExportStatuses = Struct
                            .create(CustomerDataExportStatuses.class);
                    AtomicReference<CustomerDataExportStatuses> updatedCustomerDataExportStatuses = new AtomicReference<>(
                            Struct.create(CustomerDataExportStatuses.class));
                    customerDataExportStatuses.setCreatedAt(Instant.now());
                    customerDataExportStatuses.setModifiedAt(Instant.now());
                    customerDataExportStatuses.setCreatedBy(token.getLogonName());
                    customerDataExportStatuses.setStatus("In Progress");
                    updatedCustomerDataExportStatuses.set(
                            customerDataExportStatusDao.createCustomerDataExportStatus(customerDataExportStatuses));
                    CompletableFuture
                            .runAsync(() -> exportCustomerDataService.downloadTenantData(subdomain, tenantDBContainerArray,
                                    tenantId, updatedCustomerDataExportStatuses.get()));

                    String response = "Job created successfully to export the customer data. Job Id is "
                            + updatedCustomerDataExportStatuses.get().getId();
                    return ResponseEntity.status(HttpStatus.OK).body(response);
                } else {
                    String errorMessage = "No database container found for the tenant " + tenantId;
                    auditLogData.put(STATUS, "Job Creation Failed");
                    auditLogData.put("Error", errorMessage);
                    
                    return errorMessage(errorMessage, HttpStatus.BAD_REQUEST);
                }
            } else {
                return errorMessage(UNABLE_TO_VERIFY_USER, HttpStatus.UNAUTHORIZED);
            }
        });
    }

    @GetMapping("/tenant/{tenantId}/job/{jobId}")
    public ResponseEntity<CustomerDataExportStatuses> jobStatus(@PathVariable("tenantId") String tenantId,
            @PathVariable("jobId") String jobId) {
        Token token = SpringSecurityContext.getToken();

        return getModifiedUser(tenantId, token).run(context -> {
            if (token.getEmail() != null) {
                auditLogData.put(STATUS, "Request for Job Status");
                auditData(tenantId, token, auditLogData);

                AtomicReference<CustomerDataExportStatuses> customerDataExportStatuses = new AtomicReference<>(
                        Struct.create(CustomerDataExportStatuses.class));
                customerDataExportStatuses.set(customerDataExportStatusDao.getCustomerDataExportStatus(jobId));

                auditLogData.put(STATUS, "Job Completed");
                auditLogData.put("Job Status", customerDataExportStatuses.get().getStatus());
                auditData(tenantId, token, auditLogData);

                return ResponseEntity.status(HttpStatus.OK).body(customerDataExportStatuses.get());
            } else {
                return errorMessage(UNABLE_TO_VERIFY_USER, HttpStatus.UNAUTHORIZED);
            }
        });
    }

    public static ResponseEntity errorMessage(String message, HttpStatus status) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.TEXT_PLAIN);

        return ResponseEntity.status(status).headers(headers).body(GenericUtils.sanitize(message));
    }

    public void auditData(String tenantId, Token token, Map<String, Object> auditLogData) {
        auditLogData.put("Tenant Id", tenantId);
        auditLogData.put("Triggered By (User Logon Name)", token.getEmail());
        auditLogData.put("Triggered By (Mail)", token.getEmail());
        auditLogDataList.add(auditLogData);
        
    }

    private RequestContextRunner getModifiedUser(String tenantId, Token token) {
        return cdsRuntime.requestContext().privilegedUser().modifyUser(user -> {
            user.setTenant(tenantId);
            user.setName(token.getEmail());
        });
    }
}
