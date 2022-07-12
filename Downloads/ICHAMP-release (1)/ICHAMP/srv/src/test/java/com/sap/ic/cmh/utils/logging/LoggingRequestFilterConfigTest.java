package com.sap.ic.cmh.utils.logging;

import com.sap.hcp.cf.logging.servlet.filter.RequestLoggingFilter;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.core.Ordered;

import javax.servlet.DispatcherType;

public class LoggingRequestFilterConfigTest {
    @InjectMocks
    private LoggingRequestFilterConfig loggingRequestFilterConfig;
    @Mock
    private FilterRegistrationBean<RequestLoggingFilter> filterRegistrationBean;
    @Mock
    private RequestLoggingFilter requestLoggingFilter;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}

    @Test
    public void testLoggingFilter(){
        filterRegistrationBean.setFilter(requestLoggingFilter);
        filterRegistrationBean.setName("request-logging");
        filterRegistrationBean.setDispatcherTypes(DispatcherType.REQUEST);
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE);
        loggingRequestFilterConfig.loggingFilter();
    }

}
