package com.sap.ic.cmh.ratelimit.config;

import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.ratelimit.service.RateLimitBucketService;
import com.sap.ic.cmh.gen.MessageKeys;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.ConsumptionProbe;
import java.io.IOException;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

@Component
@Slf4j
public class RatelimitingFilter implements Filter {

    @Autowired
    UserInfo userInfo;
    @Autowired
    RateLimitBucketService rateLimitBucketService;
    @Autowired
    LocaleMessageHelper localeMessageHelper;

    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        if (HttpServletRequest.class.isAssignableFrom(request.getClass())
                && HttpServletResponse.class.isAssignableFrom(response.getClass())) {
            this.doFilterRequest(
                    (HttpServletRequest) request, (HttpServletResponse) response, chain);
        } else {
            chain.doFilter(request, response);
        }
    }

    private void doFilterRequest(
            HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        if (userInfo.getTenant() != null) {
            Bucket bucket = rateLimitBucketService.resolveBucket(userInfo.getTenant());
            ConsumptionProbe probe = bucket.tryConsumeAndReturnRemaining(1);
            if (probe.isConsumed()) {
                log.debug(
                        "Remaining Rate-Limiting Tokens: "
                                + probe.getRemainingTokens()
                                + " for Tenant: "
                                + userInfo.getTenant());
                if (chain != null) {
                    chain.doFilter(request, response);
                }
            } else {
                response.sendError(
                        HttpStatus.TOO_MANY_REQUESTS.value(),localeMessageHelper.getMessage(MessageKeys.RATELIMIT_REACHED));
            }
        } else if (chain != null) {
            chain.doFilter(request, response);
        }
    }
}
