package com.sap.ic.cmh.pdm;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.context.MessageSource;

public class PDMExceptionTest {
    @Mock
    PDMException exception;
    @InjectMocks
    PDMException client;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setExceptionTest(){
    }
}
