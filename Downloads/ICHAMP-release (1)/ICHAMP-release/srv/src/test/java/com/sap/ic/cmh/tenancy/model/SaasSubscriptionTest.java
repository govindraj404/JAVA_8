package com.sap.ic.cmh.tenancy.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

public class SaasSubscriptionTest {
    @InjectMocks
    SaasSubscription subscription;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);

    }
    @Test
    public void setMethod(){
        Subscription sub=new Subscription();
        List<Subscription> list=new ArrayList<>();
        list.add(sub);
        subscription.getSubscriptions();
    }
}
