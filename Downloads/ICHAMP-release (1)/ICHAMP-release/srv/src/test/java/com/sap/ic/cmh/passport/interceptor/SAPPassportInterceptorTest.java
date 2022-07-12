package com.sap.ic.cmh.passport.interceptor;

import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.passport.PassportContext;
import com.sap.ic.cmh.passport.PassportHelper;
import com.sap.jdsr.passport.DSRPassport;
import com.sap.jdsr.util.ConvertHelper;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.protocol.HttpContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SAPPassportInterceptorTest {
    @InjectMocks
    SAPPassportInterceptor filter;
    @Mock
    PassportContext passportContext;
    @Mock
    UserInfo userInfo;
    @Mock
    ServletRequest request;
    @Mock
    ServletResponse response;
    @Mock
    FilterChain filterChain;
    @Mock
    HttpRequest httpServletRequest;
    @Mock
    HttpServletResponse httpServletResponse;
    @Mock
    PassportHelper passportHelper;
    @Mock
    DSRPassport dsrPassport=new DSRPassport();
    @Mock
    HttpContext httpContext;
    @Mock
    ConvertHelper  convertHelper;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test(expected = Exception.class)
    public void processTest() throws ServletException, IOException, HttpException {
        DSRPassport dsrPassport=new DSRPassport();
        byte[] myvar = "Any String you want".getBytes();
        String str = "fd00000aa8660b5b010006acdc0100000101000100010000";
        Optional<DSRPassport> dsrPassport1=Optional.of(dsrPassport);
        when(passportHelper.readPassportFromHexString(str)).thenReturn(dsrPassport1);
        filter.process(httpServletRequest,httpContext);
    }
    @Test
    public void processElseTest() throws ServletException, IOException, HttpException {
               filter.process(httpServletRequest,httpContext);
    }
}
