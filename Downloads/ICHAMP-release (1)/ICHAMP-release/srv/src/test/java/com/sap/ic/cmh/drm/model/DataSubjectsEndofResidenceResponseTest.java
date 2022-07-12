package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataSubjectsEndofResidenceResponseTest {

    @InjectMocks
    DataSubjectsEndofResidenceResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<DataSubjectsEndofResidenceResponse> constructor = DataSubjectsEndofResidenceResponse.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }
    @Test
    public void setMethodTest(){
        subject.setExpiredDataSubjects(new HashSet<>());
        subject.setDataSubjectBadRequests(new HashSet<>());

    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectBadRequests();
        subject.getExpiredDataSubjects();

    }

    @Test
    public void testEqualAndHashCode() {
        DataSubjectsEndofResidenceResponse s1 = new DataSubjectsEndofResidenceResponse(subject.getDataSubjectBadRequests(),
                subject.getExpiredDataSubjects());
        DataSubjectsEndofResidenceResponse  s2 = new DataSubjectsEndofResidenceResponse(subject.getDataSubjectBadRequests(),
                subject.getExpiredDataSubjects());
        assertEquals(s1, s2);
        assertTrue( s1.hashCode()==s2.hashCode());
    }
}
