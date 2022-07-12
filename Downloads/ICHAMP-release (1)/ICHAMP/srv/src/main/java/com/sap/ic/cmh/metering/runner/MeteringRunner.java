package com.sap.ic.cmh.metering.runner;

import com.sap.ic.cmh.metering.service.MeteringService;
import com.sap.ic.cmh.utils.Callable;
import org.springframework.context.ApplicationContext;

public class MeteringRunner implements Callable {

    ApplicationContext context;

    public MeteringRunner(ApplicationContext context) {
        this.context = context;
    }

    @Override
    public void call() {
        context.getBean(MeteringService.class).send();
    }
}
