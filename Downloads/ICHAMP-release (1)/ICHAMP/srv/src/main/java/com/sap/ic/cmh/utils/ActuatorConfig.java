package com.sap.ic.cmh.utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

@Configuration
@EnableWebSecurity
@Order(1) // needs to have higher priority than CAP security config
public class ActuatorConfig extends WebSecurityConfigurerAdapter{
    
  private static final Logger LOGGER = LoggerFactory.getLogger(ActuatorConfig.class);
  public static final String POLICY = "default-src 'self'";

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        LOGGER.info("*******************CONFIGURING Actuator Security*******************");
        http.headers().contentSecurityPolicy(POLICY);
        http.requestMatchers().mvcMatchers("/actuator/health").and().authorizeRequests().anyRequest().permitAll();
    }
}
