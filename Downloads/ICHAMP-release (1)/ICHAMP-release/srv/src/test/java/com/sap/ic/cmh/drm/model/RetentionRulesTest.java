package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.*;

public class RetentionRulesTest {

    @InjectMocks
    RetentionRule subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<RetentionRule> constructor = RetentionRule.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setLegalEntity("Business Partners");
        subject.setRetentionPeriod(23);
        subject.setRetentionUnit("er");
    }

    @Test
    public void getMethodTest(){
        subject.getLegalEntity();
        subject.getRetentionPeriod();
        subject.getRetentionUnit();
    }

    @Test
    public void testEqualAndHashCode() {

        RetentionRule  subject1 = new RetentionRule();
        subject1.setLegalEntity("Business Partners");
        subject1.setRetentionPeriod(23);
        subject1.setRetentionUnit("er");

        RetentionRule  subject2 = new RetentionRule();
        subject2.setLegalEntity("Business Partners");
        subject2.setRetentionPeriod(23);
        subject2.setRetentionUnit("er");

        RetentionRule  subject3 = new RetentionRule();
        subject3.setLegalEntity(null);
        subject3.setRetentionPeriod(null);
        subject3.setRetentionUnit(null);

        RetentionRule  subject4 = new RetentionRule();
        subject4.setLegalEntity("Businecss Partners");
        subject4.setRetentionPeriod(123);
        subject4.setRetentionUnit("ecr");
        subject4.toString();
        assertEquals(subject1, subject2);
        assertTrue( subject1.hashCode()==subject2.hashCode());
        assertTrue( subject1.getLegalEntity().hashCode()==subject2.getLegalEntity().hashCode());
        assertTrue( subject1.getRetentionPeriod().hashCode()==subject2.getRetentionPeriod().hashCode());
        assertTrue( subject1.getRetentionUnit().hashCode()==subject2.getRetentionUnit().hashCode());

        assertFalse( subject3.equals(subject2));

        assertFalse( subject4.getLegalEntity().hashCode()==subject2.getLegalEntity().hashCode());
        assertFalse( subject4.getRetentionPeriod().hashCode()==subject2.getRetentionPeriod().hashCode());
        assertFalse( subject4.getRetentionUnit().hashCode()==subject2.getRetentionUnit().hashCode());

    }
}
