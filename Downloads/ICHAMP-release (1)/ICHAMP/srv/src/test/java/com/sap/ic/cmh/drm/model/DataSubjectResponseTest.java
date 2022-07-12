package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataSubjectResponseTest {

    @InjectMocks
    DataSubjectResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectResponse> constructor = DataSubjectResponse.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setDataSubjectID("true");

    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectID();

    }
    @Test
    public void testEqualAndHashCode() {
        DataSubjectResponse s1 = new DataSubjectResponse("12");
        DataSubjectResponse  s2 = new DataSubjectResponse("12");
        assertEquals(s1, s2);
        assertTrue( s1.hashCode()==s2.hashCode());
    }
}
