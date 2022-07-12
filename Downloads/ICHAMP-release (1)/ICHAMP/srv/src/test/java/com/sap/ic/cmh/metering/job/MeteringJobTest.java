package com.sap.ic.cmh.metering.job;

import com.sap.ic.cmh.metering.controller.MeteringController;
import com.sap.ic.cmh.metering.service.MeteringService;
import com.sap.ic.cmh.utils.taskrunner.TenantTaskRunner;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationContext;

public class MeteringJobTest {

    @InjectMocks
    MeteringJob client;
    @Mock
    TenantTaskRunner tenantTaskRunner;
    @Mock
    ApplicationContext context;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void sendMeteringDataTest() {
        client.scheduleTestMeteringUpdates();
    }
}
