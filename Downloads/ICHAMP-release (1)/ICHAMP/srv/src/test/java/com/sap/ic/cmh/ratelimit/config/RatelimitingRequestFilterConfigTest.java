package com.sap.ic.cmh.ratelimit.config;

import io.github.bucket4j.Bucket;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import javax.servlet.ServletException;
import java.io.IOException;

public class RatelimitingRequestFilterConfigTest {
    @InjectMocks
    private RatelimitingRequestFilterConfig handler;
    @Mock
    RatelimitingFilter ratelimitingFilter;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void rateLimitingFilterTest()  {
        handler.rateLimitingFilter();

    }
}