package com.sap.ic.cmh.metering.runner;

import com.sap.ic.cmh.metering.job.MeteringJob;
import com.sap.ic.cmh.metering.service.MeteringService;
import com.sap.ic.cmh.utils.taskrunner.TenantTaskRunner;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationContext;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class MeteringRunnerTest {
    @InjectMocks
    MeteringRunner client;

    @Mock
    ApplicationContext context;
    @Mock
    MeteringService obj;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test(expected = Exception.class)
    public void runExp() {
        when(context.getBean((String) any())).thenReturn(any());
        client.call();
    }
    @Test
    public void run() {
       when(context.getBean(MeteringService.class)).thenReturn(obj);
        client.call();
    }
}
