package com.sap.ic.cmh.tenancy.handler;

import com.sap.cds.services.mt.MtUnsubscribeEventContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UnsubscriptionHandlerTest {
    @InjectMocks
    UnsubscriptionHandler unsubscriptionHandler;
    @Mock
    MtUnsubscribeEventContext contex;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
 @Test(expected = Exception.class)
    public void beforeUnsubscribeTest() {

     unsubscriptionHandler.beforeUnsubscribe(contex);
     Map<String, Object> auditLogData = new HashMap<>();
     List<Map<String, Object>> auditLogDataList = new ArrayList<>();
     contex.setTenantId("rgr");
     auditLogDataList.add(auditLogData);
     contex.setDelete(true);
     unsubscriptionHandler.beforeUnsubscribe(contex);
    }
}
