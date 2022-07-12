package com.sap.ic.cmh.utils;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class QnValidationTest {
    @InjectMocks
    QnValidation qnValidation;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);}

    @Test
    public void getQnMapTest(){
        qnValidation.getQnMap();
    }

    @Test
    public void removeQnMapTest(){
        qnValidation.removeQnMap("test");
    }
}
