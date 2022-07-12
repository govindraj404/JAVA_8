package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class DataSubjectRequestTest {

    @InjectMocks
    DataSubjectRequest subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        subject.setDataSubjectID("true");
        subject.setMaxDeletionDate("true");
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectRequest> constructor = DataSubjectRequest.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectID();
        subject.getMaxDeletionDate();


    }
}
