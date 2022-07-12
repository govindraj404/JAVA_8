package com.sap.ic.cmh.passport.interceptor;

import com.sap.cds.services.request.UserInfo;
import com.sap.ic.cmh.passport.PassportContext;
import com.sap.ic.cmh.passport.PassportHelper;
import com.sap.jdsr.passport.DSRPassport;
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

public class SAPPassportFilterTest {
    @InjectMocks
    SAPPassportFilter filter;
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
    HttpServletRequest httpServletRequest;
    @Mock
    HttpServletResponse httpServletResponse;
    @Mock
    PassportHelper  passportHelper;
    @Mock
    DSRPassport dsrPassport;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void doFilterTest() throws ServletException, IOException {
        filter.doFilter(httpServletRequest,response,filterChain);
    }
    @Test(expected = Exception.class)
    public void doFilterElseTest() throws ServletException, IOException {
       String  s="test";
        httpServletRequest.setAttribute("sap-passport","test");
        filter.doFilter(httpServletRequest,httpServletResponse,filterChain);
    }

}
