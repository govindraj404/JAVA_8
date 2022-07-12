package com.sap.ic.cmh.passport;

import com.sap.jdsr.passport.DSRPassport;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PassportContextTest {
    @InjectMocks
    PassportContext context;
    @Mock
    DSRPassport  dsrPassport;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void setTest(){
        context.set(dsrPassport);
    }
    @Test
    public void resetTest(){
        context.reset();
    }
    @Test
    public void getTest(){
        context.get();
    }

}
