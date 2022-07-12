package com.sap.ic.cmh.ratelimit.service;

import io.github.bucket4j.Bucket;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class RateLimitBucketServiceTest {
    @InjectMocks
    private RateLimitBucketService handler;
    @Mock
    Bucket bucket;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}
    @Test
    public void resolveBucketTest(){
        handler.resolveBucket("test");

    }
}
