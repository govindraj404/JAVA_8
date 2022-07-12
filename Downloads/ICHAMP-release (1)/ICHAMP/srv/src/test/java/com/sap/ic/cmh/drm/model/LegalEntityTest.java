package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class LegalEntityTest {

    @InjectMocks
    LegalEntity subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        subject.setLegalEntity("new HashSet<>()"); }

    @Test
    public void getMethodTest(){
        subject.getLegalEntity();

    }

    @Test
    public void testEqualAndHashCode() {

        LegalEntity  subject1 = new LegalEntity();
        subject1.setLegalEntity("Complaints");
        subject1.toString();

        LegalEntity  subject2 = new LegalEntity();
        subject2.setLegalEntity("Complaints");
        assertEquals(subject1, subject2);
        assertTrue( subject1.hashCode()==subject2.hashCode());
    }
}
