package com.sap.ic.cmh.passport.interceptor;

import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.passport.PassportContext;
import com.sap.ic.cmh.passport.PassportHelper;
import com.sap.ic.cmh.utils.Constants;
import com.sap.jdsr.passport.DSRPassport;
import java.io.IOException;
import java.util.Optional;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SAPPassportFilter implements Filter {

    @Autowired private PassportContext passportContext;

    @Autowired UserInfo userInfo;

    @Override
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

    private void doFilterRequest(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        String passportHexValue = request.getHeader(Constants.SAP_PASSPORT);
        // read PP from HexString
        Optional<DSRPassport> passport = PassportHelper.readPassportFromHexString(passportHexValue);
        if (!passport.isPresent()) {
            log.info("No passport was sent with the request. One will be created.");
            String method = request.getMethod();
            String url = request.getRequestURI();
            String action = method + " " + url;
            // create local PP
            passport = Optional.of(PassportHelper.createLocalPassport(action, userInfo.getId()));
        } else {
            log.info("A passport was sent with the request.");
        }

        // Set the passport in thread context
        passportContext.set(passport.get());
        MDC.put(Constants.SAP_PASSPORT, PassportHelper.createPassportHexString(passportContext.get()));

        if (chain != null) {
            chain.doFilter(request, response);
        }

        // Remove the passport DSR fields
        MDC.remove(Constants.DSR_CONNECTION_KEY);
        MDC.remove(Constants.DSR_COUNTER_KEY);
        MDC.remove(Constants.DSR_ROOT_CONTEXT_ID_KEY);
        MDC.remove(Constants.DSR_TRANSACTION_KEY);
        MDC.remove(Constants.DSR_PREV_COMP);

        // Clear the ThreadLocal.
        passportContext.reset();
    }
}
