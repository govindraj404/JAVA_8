package com.sap.ic.cmh.drm.exceptions;

import com.sap.ic.cmh.drm.model.DataSubjectRequest;
import com.sap.ic.cmh.drm.service.RetentionManagerService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class InputValidationExceptionTest {
    @InjectMocks
    private InputValidationException service;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void Test(){
    }
}
