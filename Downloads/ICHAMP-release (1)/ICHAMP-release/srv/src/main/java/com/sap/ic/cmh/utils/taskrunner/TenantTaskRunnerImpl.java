package com.sap.ic.cmh.utils.taskrunner;

import com.sap.cds.services.mt.TenantProviderService;
import com.sap.cds.services.request.RequestContext;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import java.util.List;
import java.util.function.Consumer;

import com.sap.ic.cmh.utils.Callable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

@Component
@Profile("cloud")
public class TenantTaskRunnerImpl implements TenantTaskRunner {
    private static final Logger logger =
            LoggerFactory.getLogger(TenantTaskRunnerImpl.class.getName());

    @Autowired
    CdsRuntime cdsRuntime;

    @Override
    public void execute(Callable callable) {
        TenantProviderService tenantProvider =
                cdsRuntime.getServiceCatalog()
                        .getService(TenantProviderService.class, TenantProviderService.DEFAULT_NAME);

        List<String> tenants = tenantProvider.readTenants();
        logger.debug("Found Tenants: " + tenants.size());

        tenants.forEach(tenant -> execute(tenant, callable));
    }

    @Override
    public void execute(String tenant, Callable callable) {
        try {
            cdsRuntime.requestContext().privilegedUser()
                    .modifyUser(user -> user.setTenant(tenant)).run(
                            (Consumer<RequestContext>) context ->  callable.call());
        } catch (ODataException e) {
            logger.error("OData Exception occurred");
        }
    }
}
