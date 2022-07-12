package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;

public class DataSubjectTest {

    @InjectMocks
    DataSubject subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
   @Test
    public void setMethodTest(){
       subject.setDataSubjectID("test");
       subject.setDataSubjectRole("test");
       subject.setLegalGround("test");
   }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectID();
        subject.getDataSubjectRole();
        subject.getLegalGround();
    }

    @Test
    public void testEqualAndHashCode() {

        DataSubject  s1 = new DataSubject();
        s1.setDataSubjectID("001");
        s1.setLegalGround("India");
        s1.setDataSubjectRole("1");
        DataSubject  s2 = new DataSubject();
        s2.setDataSubjectID("001");
        s2.setLegalGround("India");
        s2.setDataSubjectRole("1");
        s2.toString();
        assertEquals(s1, s2);
        assertTrue( s1.hashCode()==s2.hashCode());
    }

    @Test
    public void testNullAndHashCode() {

        DataSubject  s1 = new DataSubject();
        s1.setDataSubjectID(" ");
        s1.setLegalGround(" ");
        s1.setDataSubjectRole(" ");
        DataSubject  s2 = new DataSubject();
        s2.setDataSubjectID("001");
        s2.setLegalGround("India");
        s2.setDataSubjectRole("1");
        s2.toString();
        assertNotEquals(s1, s2);
        assertFalse( s1.hashCode()==s2.hashCode());
    }
}
