package com.sap.ic.cmh.ratelimit.config;

import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.ratelimit.service.RateLimitBucketService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.ConsumptionProbe;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class RatelimitingFilterTest {
    @InjectMocks
    private RatelimitingFilter handler;
    @Mock
    Bucket bucket;
    @Mock
    UserInfo userInfo;
    @Mock
    RateLimitBucketService rateLimitBucketService;
    @Mock
    LocaleMessageHelper localeMessageHelper;
    @Mock
    HttpServletRequest  request;
    @Mock
    HttpServletResponse response;
    @Mock
    ServletRequest  httprequest;
    @Mock
    ServletResponse httpresponse;
    @Mock
    FilterChain chain;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}
    @Test
    public void doFilterTest() throws ServletException, IOException {
        handler.doFilter(request,response,chain);

    }
    @Test
    public void doFilterElseTest() throws ServletException, IOException {
        handler.doFilter(httprequest,httpresponse,chain);

    }
    @Mock
    ConsumptionProbe probe;

    @Test
    public void doFilterRequestTest() throws ServletException, IOException {
        when(userInfo.getTenant()).thenReturn("test");
        when(rateLimitBucketService.resolveBucket(any())).thenReturn(bucket);
        when(bucket.tryConsumeAndReturnRemaining(1l)).thenReturn(probe);
        handler.doFilter(request,response,chain);

    }
    @Test
    public void doFilterRequestElseTest() throws ServletException, IOException {
        when(userInfo.getTenant()).thenReturn("test");
        when(rateLimitBucketService.resolveBucket(any())).thenReturn(bucket);
        when(bucket.tryConsumeAndReturnRemaining(1l)).thenReturn(probe);
        when(probe.isConsumed()).thenReturn(true);
        handler.doFilter(request,response,chain);

    }
}
