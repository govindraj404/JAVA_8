package com.sap.ic.cmh.objectstore.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class UploadStatusTest {
    @InjectMocks
    UploadStatus uploadStatus;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void setMethodTest(){
        uploadStatus.setStatus("test");
        uploadStatus.setErrorMessage("test");
    }
    @Test
    public void getMethodTest(){
        uploadStatus.getStatus();
        uploadStatus.getErrorMessage();
    }

}
