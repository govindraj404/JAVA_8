package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class ValueHelpResponseTest {

    @InjectMocks
    ValueHelpResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }


    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<ValueHelpResponse> constructor = ValueHelpResponse.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setValue("new HashSet<>()");
    subject.setValueDesc("test");}

    @Test
    public void getMethodTest(){
        subject.getValue();
        subject.getValueDesc();

    }
}
