package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataSubjectLastRetentionStartDatesResponseTest {

    @InjectMocks
    DataSubjectLastRetentionStartDatesResponse subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        subject.setRetentionID("true");
        subject.setRetentionStartDate("test");

    }

    @Test
    public void getMethodTest(){
        subject.getRetentionID();
        subject.getRetentionStartDate();

    }

    @Test
    public void testEqualAndHashCode() {

        DataSubjectLastRetentionStartDatesResponse  subject1 = new DataSubjectLastRetentionStartDatesResponse("true", "2021-09-09");
        DataSubjectLastRetentionStartDatesResponse  subject2 = new DataSubjectLastRetentionStartDatesResponse("true", "2021-09-09");
        assertEquals(subject1, subject2);
        subject1.toString();
        assertTrue( subject1.hashCode()==subject2.hashCode());
    }
}
