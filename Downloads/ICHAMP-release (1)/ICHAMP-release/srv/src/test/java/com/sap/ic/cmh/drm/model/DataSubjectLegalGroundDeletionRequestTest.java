package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class DataSubjectLegalGroundDeletionRequestTest {

    @InjectMocks
    DataSubjectLegalGroundDeletionRequest subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        subject.setDataSubjectID("true");
        subject.setDataSubjectRole("true");
        subject.setDataSubjectRole("true");
        subject.setStartTime("true");
        subject.setRetentionRules(new ArrayList<>());

    }

    @Test
    public void getMethodTest(){
        subject.getDataSubjectID();
        subject.getDataSubjectRole();
        subject.getLegalGround();
        subject.getMaxDeletionDate();
        subject.getStartTime();
        subject.getRetentionRules();

    }

    @Test
    public void testEqualAndHashCode() {

        DataSubjectLegalGroundDeletionRequest  subject1 = new DataSubjectLegalGroundDeletionRequest();
        subject1.setDataSubjectID("true");
        subject1.setDataSubjectRole("test");
        subject1.setLegalGround("test");
        subject1.setMaxDeletionDate(null);
        subject1.setStartTime(null);
        subject1.setRetentionRules(new ArrayList<>());

        DataSubjectLegalGroundDeletionRequest  subject2 = new DataSubjectLegalGroundDeletionRequest();
        subject2.setDataSubjectID("true");
        subject2.setDataSubjectRole("test");
        subject2.setLegalGround("test");
        subject2.setMaxDeletionDate(null);
        subject2.setStartTime(null);
        subject2.setRetentionRules(new ArrayList<>());
        assertEquals(subject1, subject2);
        subject2.toString();
        assertTrue( subject1.hashCode()==subject2.hashCode());
    }

    @Test
    public void testNotEqualAndHashCode() {

        DataSubjectLegalGroundDeletionRequest  subject1 = new DataSubjectLegalGroundDeletionRequest();
        subject1.setDataSubjectID(null);
        subject1.setDataSubjectRole(null);
        subject1.setLegalGround(null);
        subject1.setMaxDeletionDate(null);
        subject1.setStartTime(null);
        subject1.setRetentionRules(new ArrayList<>());

        DataSubjectLegalGroundDeletionRequest  subject2 = new DataSubjectLegalGroundDeletionRequest();
        subject2.setDataSubjectID("true");
        subject2.setDataSubjectRole("test");
        subject2.setLegalGround("test");
        subject2.setMaxDeletionDate(null);
        subject2.setStartTime(null);
        subject2.setRetentionRules(new ArrayList<>());
        assertNotEquals(subject1, subject2);
        subject2.toString();
        assertFalse( subject1.hashCode()==subject2.hashCode());
    }
}
