package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.*;

public class RulesConditionTest {

    @InjectMocks
    RulesCondition subject;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<RulesCondition> constructor = RulesCondition.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest() {
        subject.setRetentionID("23");
    }

    @Test
    public void getMethodTest() {
        subject.getRetentionID();

    }

    @Test
    public void testEqualAndHashCode() {

        RulesCondition subject1 = new RulesCondition();
        subject1.setRetentionID("23");

        RulesCondition subject2 = new RulesCondition();
        subject2.setRetentionID("23");

        RulesCondition subject3 = new RulesCondition();
        subject3.setRetentionID(null);

        RulesCondition subject4 = new RulesCondition();
        subject4.setRetentionID("123");
        RulesCondition subject5 = null;
                subject2.toString();
        subject4.toString();
        assertEquals(subject1, subject2);
        assertNotEquals(subject3, subject2);
        assertNotEquals(subject4, subject2);
        assertTrue(this.hashCode() == this.hashCode());
        assertTrue(subject1.hashCode() == subject2.hashCode());
        assertTrue(subject1.getRetentionID().hashCode() == subject2.getRetentionID().hashCode());
        assertTrue(subject1.getClass() == subject2.getClass());
        assertFalse(null == " ");
        assertFalse(subject4.getClass() != subject2.getClass());
        assertFalse(subject3.hashCode() == subject2.hashCode());
        assertFalse(subject4.getRetentionID().hashCode() == subject2.getRetentionID().hashCode());
        assertFalse(subject3.equals(subject2));
        assertFalse(subject4.hashCode() == subject2.hashCode());
    }}