package com.sap.ic.cmh.utils.taskrunner;

import com.sap.cds.Struct;
import com.sap.cds.services.mt.TenantProviderService;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.metering.runner.MeteringRunner;
import com.sap.ic.cmh.utils.Callable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

import java.util.List;

import static org.mockito.Mockito.when;

public class TenantTaskRunnerImplTest {
    @InjectMocks
    @Autowired
    private TenantTaskRunnerImpl tenantTaskRunnerImpl;
    @Mock
    private  TenantTaskRunner tenantTaskRunner;
    @Mock
    private CdsRuntime cdsRuntime;
    @Mock
    private Callable callable;
    @Mock
    private TenantProviderService tenantProviderService;
    @Mock
    private ApplicationContext context;
    private List<String> tenants;
    String tenant;
    private static final Logger logger =
            LoggerFactory.getLogger(TenantTaskRunnerImpl.class.getName());

    @Before
    public void beforeClass() {
        tenantTaskRunner = Struct.create(TenantTaskRunner.class);
        tenantProviderService=Struct.create(TenantProviderService.class);
        cdsRuntime=Struct.create(CdsRuntime.class);
    }

    @Test
    public void testExecute(){
        tenants = tenantProviderService.readTenants();
        tenantTaskRunner.execute(callable);
        MeteringRunner meteringRunner = new MeteringRunner(context);
        tenantTaskRunner=new TenantTaskRunnerImpl();
    }

    @Test(expected = Exception.class)
    public  void testExecuteForTenantAndRunnable(){
        try{
            cdsRuntime.requestContext().privilegedUser()
                    .modifyUser(user -> user.setTenant(tenant)).run(
                            context -> { callable.call(); });
            tenantTaskRunnerImpl.execute(tenant,callable);
        }
        catch (Exception e){
            when(cdsRuntime.requestContext().privilegedUser()).thenThrow(Exception.class);
            logger.error(e.getMessage(), e);
        }
    }
}
