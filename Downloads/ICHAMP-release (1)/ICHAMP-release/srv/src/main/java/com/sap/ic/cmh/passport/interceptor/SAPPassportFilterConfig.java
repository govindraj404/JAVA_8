package com.sap.ic.cmh.passport.interceptor;

import javax.servlet.DispatcherType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;

@Configuration
public class SAPPassportFilterConfig {

    @Autowired SAPPassportFilter sapPassportFilterDetails;

    @Bean
    public FilterRegistrationBean<SAPPassportFilter> sapPassportFilter() {
        FilterRegistrationBean<SAPPassportFilter> filterRegistrationBean = new FilterRegistrationBean<>();
        filterRegistrationBean.setFilter(sapPassportFilterDetails);
        filterRegistrationBean.setName("sap-passport");
        filterRegistrationBean.setDispatcherTypes(DispatcherType.REQUEST);
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return filterRegistrationBean;
    }
}
