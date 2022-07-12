package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;

public class DataSubjectEndOfBusinessResponseTest {
    @InjectMocks
    DataSubjectEndOfBusinessResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        subject.setDataSubjectExpired(true);
        subject.setDataSubjectNotExpiredReason("test");
    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectExpired();
        subject.getDataSubjectNotExpiredReason();
    }

    @Test
    public void testEqualAndHashCode() {

        DataSubjectEndOfBusinessResponse  s1 = new DataSubjectEndOfBusinessResponse(true, "sdfg");
        DataSubjectEndOfBusinessResponse  s2 = new DataSubjectEndOfBusinessResponse(true, "sdfg");
        assertEquals(s1, s2);
        s1.toString();
        assertTrue( s1.hashCode()==s2.hashCode());
    }
    @Test
    public void testEqualAndHashCodefalse() {

        DataSubjectEndOfBusinessResponse  s1 = new DataSubjectEndOfBusinessResponse(false, "sdfg");
        DataSubjectEndOfBusinessResponse  s2 = new DataSubjectEndOfBusinessResponse(false, "sdfg");
        assertEquals(s1, s2);
        s1.toString();
        assertFalse(s1.hashCode() !=s2.hashCode());
    }


}
