package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataSubjectResidenceTest {

    @InjectMocks
    DataSubjectResidence subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectResidence> constructor = DataSubjectResidence.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setDataSubjectId("true");
        subject.setDataSubjectReferenceDate("true");
    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectId();
        subject.getDataSubjectReferenceDate();


    }
    @Test
    public void testEqualAndHashCode() {
        DataSubjectResidence  s1 = new DataSubjectResidence("12", "2-4-2011");
        DataSubjectResidence  s2 = new DataSubjectResidence("12", "2-4-2011");
        assertEquals(s1, s2);
        assertTrue( s1.hashCode()==s2.hashCode());
    }
}
