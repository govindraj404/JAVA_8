package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataSubjectLastRetentionStartDatesRequestTest {

        @InjectMocks
        DataSubjectLastRetentionStartDatesRequest subject;
        @Before
        public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
        @Test
        public void setMethodTest(){
        subject.setDataSubjectID("true");
        subject.setDataSubjectRole("test");
        subject.setLegalGround("test");
        subject.setLegalEntity("test");
        subject.setRulesConditionSet(new ArrayList<>());
        subject.setStartTime("");    }

        @Test
        public void getMethodTest(){
        subject.getDataSubjectID();
        subject.getDataSubjectRole();
        subject.getStartTime();
        subject.getLegalGround();
        subject.getRulesConditionSet();
        subject.getLegalEntity();

    }

    @Test
    public void testEqualAndHashCode() {

        DataSubjectLastRetentionStartDatesRequest  subject1 = new DataSubjectLastRetentionStartDatesRequest();
        subject1.setDataSubjectID("true");
        subject1.setDataSubjectRole("test");
        subject1.setLegalGround("test");
        subject1.setLegalEntity("test");
        subject1.setRulesConditionSet(new ArrayList<>());
        DataSubjectLastRetentionStartDatesRequest  subject2 = new DataSubjectLastRetentionStartDatesRequest();
        subject2.setDataSubjectID("true");
        subject2.setDataSubjectRole("test");
        subject2.setLegalGround("test");
        subject2.setLegalEntity("test");
        subject2.setRulesConditionSet(new ArrayList<>());
        subject2.toString();
        assertEquals(subject1, subject2);
        assertTrue( subject1.hashCode()==subject2.hashCode());
    }
}
