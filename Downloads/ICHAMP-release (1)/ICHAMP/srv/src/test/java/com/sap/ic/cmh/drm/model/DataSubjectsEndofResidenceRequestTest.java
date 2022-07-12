package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

public class DataSubjectsEndofResidenceRequestTest {

    @InjectMocks
    DataSubjectsEndofResidenceRequest subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectsEndofResidenceRequest> constructor = DataSubjectsEndofResidenceRequest.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }
    @Test
    public void setMethodTest(){
        subject.setLegalGround("true");
        subject.setLegalGroundResidenceRules(new ArrayList<>());

    }

    @Test
    public void getMethodTest(){
        subject.getLegalGround();
        subject.getLegalGroundResidenceRules();

    }
}
