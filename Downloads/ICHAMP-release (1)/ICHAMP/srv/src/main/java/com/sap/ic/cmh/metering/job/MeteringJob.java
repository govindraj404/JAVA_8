package com.sap.ic.cmh.metering.job;

import com.sap.ic.cmh.metering.runner.MeteringRunner;
import com.sap.ic.cmh.utils.taskrunner.TenantTaskRunner;
import org.joda.time.Instant;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class MeteringJob {

    @Autowired
    TenantTaskRunner tenantTaskRunner;

    private final Logger logger = LoggerFactory.getLogger(MeteringJob.class);

    @Autowired
    ApplicationContext context;

    @Scheduled(cron = "0 0 2 * * *")
    public void scheduleTestMeteringUpdates() {
        logger.debug("Running Metering Job : " + Instant.now());
        MeteringRunner meteringRunner = new MeteringRunner(context);
        tenantTaskRunner.execute(meteringRunner);
    }
}