package com.sap.ic.cmh.passport.interceptor;

import com.sap.ic.cmh.passport.PassportHelper;
import com.sap.ic.cmh.utils.Constants;
import com.sap.jdsr.passport.DSRPassport;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestInterceptor;
import org.apache.http.protocol.HttpContext;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Optional;

@Component
public class SAPPassportInterceptor implements HttpRequestInterceptor {

    @Override
    public void process(HttpRequest request, HttpContext context)
            throws HttpException, IOException {
        Optional<DSRPassport> optionalDSRPassport = PassportHelper.readPassportFromHexString(MDC.get(Constants.SAP_PASSPORT));
        if (optionalDSRPassport.isPresent()) {
            DSRPassport localPassport = optionalDSRPassport.get();
            DSRPassport remotePassport = PassportHelper.createRemotePassport(localPassport);
            String remotePassportHex = PassportHelper.createPassportHexString(remotePassport);
            request.setHeader(Constants.SAP_PASSPORT, remotePassportHex);
        }
    }

}
