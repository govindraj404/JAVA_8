package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

import static org.junit.Assert.assertTrue;

public class DataSubjectLegalEntitiesResponseTest {

    @InjectMocks
    DataSubjectLegalEntitiesResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectLegalEntitiesResponse> constructor = DataSubjectLegalEntitiesResponse.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }
    @Test
    public void setMethodTest(){
        subject.setLegalEntity("true");

    }

    @Test
    public void getMethodTest(){
        subject.getLegalEntity();
    }
}
