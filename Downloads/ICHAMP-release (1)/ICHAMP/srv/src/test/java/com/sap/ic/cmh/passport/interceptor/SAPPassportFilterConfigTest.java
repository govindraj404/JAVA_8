package com.sap.ic.cmh.passport.interceptor;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class SAPPassportFilterConfigTest {
    @InjectMocks
    SAPPassportFilterConfig filter;
    @Mock
    SAPPassportFilter sapPassportFilter;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void sapPassportFilterTest()  {
        filter.sapPassportFilter();
    }
}
