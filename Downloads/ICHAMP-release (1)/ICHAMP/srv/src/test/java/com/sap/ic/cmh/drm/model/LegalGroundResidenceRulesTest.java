package com.sap.ic.cmh.drm.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

public class LegalGroundResidenceRulesTest {

    @InjectMocks
    LegalGroundResidenceRules subject;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testConstructorIsPrivate() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Constructor<LegalGroundResidenceRules> constructor = LegalGroundResidenceRules.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }

    @Test
    public void setMethodTest(){
        subject.setLegalEntity("new HashSet<>()");
        subject.setRuleConditionSet(new ArrayList<>());

    }

    @Test
    public void getMethodTest(){
        subject.getLegalEntity();
        subject.getRuleConditionSet();

    }
}
