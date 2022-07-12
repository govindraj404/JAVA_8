package com.sap.ic.cmh.metering.service;

import cds.gen.com.sap.ic.cmh.complaint.Complaints;
import cds.gen.com.sap.ic.cmh.complaint.Complaints_;
import cds.gen.com.sap.ic.cmh.meteringlog.MeteringLogs;
import cds.gen.com.sap.ic.cmh.meteringlog.MeteringLogs_;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Upsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpsert;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.metering.client.MeteringClient;
import com.sap.ic.cmh.tenancy.model.SaasSubscription;
import com.sap.ic.cmh.tenancy.model.Subscription;
import com.sap.ic.cmh.tenancy.service.TenantSubscriptionService;
import com.sap.ic.cmh.utils.Constants;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.context.annotation.Profile;
import java.time.Instant;
import java.util.List;

@Service
@Profile("cloud")
public class MeteringServiceImpl implements MeteringService {
    /*
     * 1. Read metering tables, find the last successful data read time and current
     * time and save the delta number of records executed. 2. Send it to metering
     * service for that tenant. 3. Save in the DB, last successful metered
     * timestamp. 4. Next time use this to find the delta number of complaints.
     */
    private final Logger logger = LoggerFactory.getLogger(MeteringServiceImpl.class);

    @Autowired
    MeteringClient meteringClient;

    @Autowired
    TenantSubscriptionService tenantSubscriptionService;

    @Autowired
    UserInfo userInfo;

    @Autowired 
    private PersistenceService db;

    @Override
    public void send() {
        Instant startTime = null;
        Instant endTime = null;
        CqnSelect selectMetering = Select.from(MeteringLogs_.class);
        Result meterResult = db.run(selectMetering);
        if (meterResult.first().isPresent()) {
            CqnSelect selectRemaining;
            MeteringLogs meteringObj = meterResult.single().as(MeteringLogs.class);
            startTime = meteringObj.getTime();
            endTime = Instant.now();
            final Instant startTimeValue = startTime;
            final Instant endTimeValue = endTime;
            selectRemaining = Select.from(Complaints_.class)
                              .where(o -> o.createdAt().between(startTimeValue, endTimeValue))
                              .orderBy(o -> o.createdAt().asc());

            Result resultRemaining = db.run(selectRemaining);
            updateMeterLogs(resultRemaining, endTime);
        } else {
            logger.debug("Metering is getting logged for the first tine");
            CqnSelect selectRemaining = Select.from(Complaints_.class).orderBy(o -> o.createdAt().asc());
            Result resultRemaining = db.run(selectRemaining);
            List<Complaints> actionStatusesOBJ = resultRemaining.listOf(Complaints.class);
            if (!CollectionUtils.isEmpty(actionStatusesOBJ)) {
                endTime = Instant.now();
                updateMeterLogs(resultRemaining, endTime);
            }
        }
    }

    private void updateMeterLogs(Result resultRemaining, Instant endTime) {
        if (resultRemaining.first().isPresent()) {
            SaasSubscription saasSubscription = tenantSubscriptionService.getSaasSubscription();
            if (saasSubscription != null
                    && !CollectionUtils.isEmpty(saasSubscription.getSubscriptions())) {
                Subscription subscriptionForCurrentTenant =
                        getSubscribedTime(userInfo.getTenant(), saasSubscription.getSubscriptions());
                boolean status =
                        meteringClient.meteringServiceClient(resultRemaining.rowCount(),subscriptionForCurrentTenant);

                CqnSelect meteringID = Select.from(MeteringLogs_.class);
                Result meteringRes = db.run(meteringID);
                MeteringLogs meteringLogs;

                if (meteringRes.first().isPresent()) {
                    meteringLogs = meteringRes.single().as(MeteringLogs.class);
                } else {
                    meteringLogs = Struct.create(MeteringLogs.class);
                }

                if (status) {
                    meteringLogs.setStatus(Constants.SUCCESS);
                    meteringLogs.setTime(endTime);
                } else {
                    meteringLogs.setStatus(Constants.FAILURE);
                }
                meteringLogs.setActionExecutionCount((int) resultRemaining.rowCount());
                CqnUpsert upsert = Upsert.into(MeteringLogs_.class).entry(meteringLogs);
                db.run(upsert);
            }
        }
    }

    private Subscription getSubscribedTime(String tenantId, List<Subscription> subscriptionList) {
        for (Subscription subscription : subscriptionList) {
            if (tenantId.equalsIgnoreCase(subscription.getConsumerTenantId())) {
                logger.debug("Tenant Found in metering ");
                return subscription;
            }
        }
        return null;
    }
}