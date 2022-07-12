package com.sap.ic.cmh.utils;

import com.sap.cds.services.runtime.CdsRuntime;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class LocaleMessageHelperTest {
    @InjectMocks
    LocaleMessageHelper localeMessageHelper;
    @Mock
    CdsRuntime runtime;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void getMessageTest()throws  Exception{
        localeMessageHelper.getMessage("httpSecurity");
    }
    @Test
    public void getMessageNullTest()throws  Exception{
        localeMessageHelper.getMessage("");
    }
}
