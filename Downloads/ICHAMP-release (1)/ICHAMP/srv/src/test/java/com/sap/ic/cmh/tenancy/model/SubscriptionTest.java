package com.sap.ic.cmh.tenancy.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class SubscriptionTest {
    @InjectMocks
    Subscription subscription;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);

    }

    @Test
    public void getMethod(){
        subscription.getConsumerTenantId();
        subscription.getInternalSubscriptionId();
        subscription.getAppName();
        subscription.getChangedOn();
        subscription.getSubdomain();
        subscription.getCreatedOn();
        subscription.getGlobalAccountId();
        subscription.getState();
        subscription.getConsumerTenantId();
        subscription.getSubaccountId();
        subscription.getState();
        subscription.getUrl();
    }
}
