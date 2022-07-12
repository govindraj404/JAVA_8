package com.sap.ic.cmh.drm.exceptions;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class NoContentExceptionTest {
    @InjectMocks
    private NoContentException service;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void Test(){
        service = new NoContentException();
        service = new NoContentException("Exception");
    }
}
