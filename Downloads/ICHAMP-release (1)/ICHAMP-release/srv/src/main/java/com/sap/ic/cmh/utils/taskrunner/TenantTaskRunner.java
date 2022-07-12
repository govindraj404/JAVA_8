package com.sap.ic.cmh.utils.taskrunner;

import com.sap.ic.cmh.utils.Callable;

public interface TenantTaskRunner {

    void execute(Callable callable);

    void execute(String tenant, Callable callable);

}
