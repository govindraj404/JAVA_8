package com.sap.ic.cmh.utils;


import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;
import org.springframework.security.config.annotation.web.configurers.HeadersConfigurer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ActuatorConfigTest {
    @InjectMocks
    ActuatorConfig actuatorConfig;
    @Mock
    HttpSecurity httpSecurity;
    @Mock
    Exception exception;
    public static final String POLICY = "default-src 'self'";
    @Before
    public void beforeClass() {MockitoAnnotations.openMocks(this);}

   @Test(expected = Exception.class)
   public void configureTest()throws  Exception{actuatorConfig.configure(httpSecurity);}

    @Test(expected = Exception.class)
    public void testConfigure() throws Exception{
        when(httpSecurity.antMatcher("/actuator/health")).thenReturn(httpSecurity.antMatcher("/*"));
        when(httpSecurity.authorizeRequests()).thenReturn(httpSecurity.authorizeRequests());
        when(httpSecurity.requestMatchers().antMatchers("/actuator/health").and().authorizeRequests().anyRequest().permitAll()).thenReturn(httpSecurity.antMatcher("/actuator/health").authorizeRequests().anyRequest().permitAll());
        actuatorConfig.configure(httpSecurity);
    }

    @Mock
    HttpSecurity.RequestMatcherConfigurer requestMatcherConfigurer;
    @Mock
    HttpSecurity.MvcMatchersRequestMatcherConfigurer mvcMatchersRequestMatcherConfigurer;
    @Mock
    ExpressionUrlAuthorizationConfigurer.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry;
    @Mock
    ExpressionUrlAuthorizationConfigurer.AuthorizedUrl authorizedUrl;
    @Mock
    HeadersConfigurer<HttpSecurity> httpSecurityHeadersConfigurer;
@Mock
    HeadersConfigurer.ContentSecurityPolicyConfig contentSecurityPolicyConfig;

    @Test//(expected = Exception.class)
    public void testConfigureForPolicy() throws Exception{
        when(httpSecurity.headers()).thenReturn(httpSecurityHeadersConfigurer);
       when(httpSecurityHeadersConfigurer.contentSecurityPolicy(any(String.class))).thenReturn(contentSecurityPolicyConfig);
        when(httpSecurity.requestMatchers()).thenReturn(requestMatcherConfigurer);
        when(requestMatcherConfigurer.mvcMatchers(any())).thenReturn(mvcMatchersRequestMatcherConfigurer);
        when(mvcMatchersRequestMatcherConfigurer.and()).thenReturn(httpSecurity);
        when(httpSecurity.authorizeRequests()).thenReturn(expressionInterceptUrlRegistry);
        when(expressionInterceptUrlRegistry.anyRequest()).thenReturn(authorizedUrl);
        actuatorConfig.configure(httpSecurity);
    }
}
