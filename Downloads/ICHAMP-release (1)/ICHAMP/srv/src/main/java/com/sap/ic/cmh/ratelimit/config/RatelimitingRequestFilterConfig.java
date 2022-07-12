package com.sap.ic.cmh.ratelimit.config;

import javax.servlet.DispatcherType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RatelimitingRequestFilterConfig {

    @Autowired RatelimitingFilter ratelimitingFilter;

    @Bean
    public FilterRegistrationBean rateLimitingFilter() {
        FilterRegistrationBean filterRegistrationBean = new FilterRegistrationBean();
        filterRegistrationBean.setFilter(ratelimitingFilter);
        filterRegistrationBean.setName("rate-limiting");
        filterRegistrationBean.setDispatcherTypes(DispatcherType.REQUEST);
        return filterRegistrationBean;
    }
}
