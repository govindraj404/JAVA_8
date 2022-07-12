package com.sap.ic.cmh.tenancy.handler;

import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.mt.MtSubscriptionService;
import com.sap.cds.services.mt.MtUnsubscribeEventContext;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Handler that implements unsubscription logic */
@Component
@Profile("cloud")
@ServiceName(MtSubscriptionService.DEFAULT_NAME)
public class UnsubscriptionHandler implements EventHandler {

    @Autowired
    AuditLogHelper auditLogHelper;

    @Before(event = MtSubscriptionService.EVENT_UNSUBSCRIBE)
    public void beforeUnsubscribe(MtUnsubscribeEventContext context) {
        Map<String, Object> auditLogData = new HashMap<>();
        List<Map<String, Object>> auditLogDataList = new ArrayList<>();
        auditLogData.put("Tenant Id", context.getTenantId());
        auditLogData.put("Triggered By (User Logon Name)", context.getUserInfo().getName());
        auditLogDataList.add(auditLogData);
        
        // Trigger deletion of database container of off-boarded tenant
        context.setDelete(true);
    }
}
