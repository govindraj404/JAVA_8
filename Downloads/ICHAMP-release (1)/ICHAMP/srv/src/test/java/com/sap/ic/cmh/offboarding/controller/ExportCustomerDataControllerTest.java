package com.sap.ic.cmh.offboarding.controller;

import com.sap.cds.Struct;
import com.sap.cds.services.request.RequestContext;
import com.sap.cds.services.request.UserInfo;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cds.services.runtime.RequestContextRunner;
import com.sap.cloud.security.xsuaa.token.SpringSecurityContext;
import com.sap.cloud.security.xsuaa.token.Token;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.objectstore.service.ObjectStoreService;
import com.sap.ic.cmh.offboarding.persistency.CustomerDataExportStatusDao;
import com.sap.ic.cmh.offboarding.service.ExportCustomerDataService;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.InputStream;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ExportCustomerDataControllerTest {
    @InjectMocks
    @Autowired
    ExportCustomerDataController controller;
    @Mock
    ExportCustomerDataService exportCustomerDataService;
    @Mock
    CdsRuntime cdsRuntime;
    @Mock
    CustomerDataExportStatusDao customerDataExportStatusDao;
    @Mock
    ObjectStoreService objectStoreService;
    @Mock
    SpringSecurityContext context;
    @Mock
    Token token;
    @Mock
    RequestContextRunner runner;
    @Mock
    JSONObject jsonObject;
    @Mock
    JSONArray array;

    @Mock
    InputStream  stream;

    @Mock
    AuditLogHelper logHelper;

    @Mock
    UserInfo userInfo;


    RequestContext requestContext;

 /*   @Mock
    CdsRuntime cdsRunTime;*/

    RequestContextRunner runner1;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        requestContext = mock(RequestContext.class);
        runner1 = mock(RequestContextRunner.class);
    }

    @Test
    public void jobStatusTest(){
        try (MockedStatic<SpringSecurityContext> mockedClass = Mockito.mockStatic(SpringSecurityContext.class)) {
            mockedClass.when(SpringSecurityContext::getToken).thenReturn(token);
            when(token.getEmail()).thenReturn("test@gmail.com");
            when(cdsRuntime.requestContext()).thenReturn(runner);
            when(runner.privilegedUser()).thenReturn(runner);
            when(runner.modifyUser(any())).thenReturn(runner);
            controller.jobStatus("test", "231");
        }
    }
    @Test
    public void jobStatusElseTest() {
        try (MockedStatic<SpringSecurityContext> mockedClass = Mockito.mockStatic(SpringSecurityContext.class)) {
            mockedClass.when(SpringSecurityContext::getToken).thenReturn(token);
            when(token.getEmail()).thenReturn("test@gmail.com");
            when(cdsRuntime.requestContext()).thenReturn(runner);
            when(runner.privilegedUser()).thenReturn(runner);
            when(runner.modifyUser(any())).thenReturn(runner);
            controller.jobStatus("test", "231");
        }
    }

    @Test
    public void exportDataTest() {
        try (MockedStatic<SpringSecurityContext> mockedClass = Mockito.mockStatic(SpringSecurityContext.class)) {
            mockedClass.when(SpringSecurityContext::getToken).thenReturn(token);
            when(token.getEmail()).thenReturn("test@gmail.com");
            when(exportCustomerDataService.getTenantDBContainers(any())).thenReturn(jsonObject);
            when(jsonObject.getJSONArray(any())).thenReturn(array);
            when(cdsRuntime.requestContext()).thenReturn(runner);
            when(runner.privilegedUser()).thenReturn(runner);
            when(runner.modifyUser(any())).thenReturn(runner);
            controller.exportData("subdomain","test");
        }
    }

    @Test
    public void exportDataElseTest(){
        try (MockedStatic<SpringSecurityContext> mockedClass = Mockito.mockStatic(SpringSecurityContext.class)) {
            mockedClass.when(SpringSecurityContext::getToken).thenReturn(token);
            CdsRuntime cdsRunTime = mock(CdsRuntime.class);
            RequestContextRunner runner =  mock(RequestContextRunner.class);
            RequestContextRunner runner1 =  mock(RequestContextRunner.class);
            runner.user(userInfo);
            when(requestContext.getUserInfo()).thenReturn(userInfo);
            when(userInfo.getName()).thenReturn("Karthik");
            when(userInfo.getTenant()).thenReturn("Karthik");
            when(token.getEmail()).thenReturn("test@gmail.com");
            runner1.user(userInfo);
            when(cdsRuntime.requestContext()).thenReturn(runner);
            when(runner.privilegedUser()).thenReturn(runner1);
            when(runner1.modifyUser(any())).thenReturn(runner1);
            when(runner.modifyUser(any())).thenReturn(runner1);

            Mockito.doAnswer(invocation -> {
                Function<RequestContext, ResponseEntity> function = invocation.getArgument(0);
                function.apply(requestContext);

                return null;
            }).when(runner1).run((Consumer<RequestContext>) Mockito.any());
            controller.exportData("subdomain","test");
        }
    }

    @Test
    public void exportDataElse1Test() {
        try (MockedStatic<SpringSecurityContext> mockedClass = Mockito.mockStatic(SpringSecurityContext.class)) {
            mockedClass.when(SpringSecurityContext::getToken).thenReturn(token);
            when(token.getEmail()).thenReturn("test@gmail.com");
            JSONArray array=new JSONArray();
            when(exportCustomerDataService.getTenantDBContainers(any())).thenReturn(jsonObject);
            when(jsonObject.getJSONArray(any())).thenReturn(array);
            when(cdsRuntime.requestContext()).thenReturn(runner);
            when(runner.privilegedUser()).thenReturn(runner);
            when(runner.modifyUser(any())).thenReturn(runner);
            controller.exportData("subdomain","test");
        }
    }


}
