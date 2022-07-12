package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

public class ResidenceRuleConditionSetTest {

    @InjectMocks
    ResidenceRuleConditionSet subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<ResidenceRuleConditionSet> constructor = ResidenceRuleConditionSet.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setConditionSet(new ArrayList<>());
        subject.setResidenceDate("test");}

    @Test
    public void getMethodTest(){
        subject.getConditionSet();
        subject.getResidenceDate();

    }
}
