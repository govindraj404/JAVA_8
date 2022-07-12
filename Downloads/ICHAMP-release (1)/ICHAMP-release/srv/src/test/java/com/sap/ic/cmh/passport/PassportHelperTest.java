package com.sap.ic.cmh.passport;

import com.sap.jdsr.passport.DSRPassport;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.UUID;

public class PassportHelperTest {
    @InjectMocks
    PassportHelper context;
    @Mock
    DSRPassport dsrPassport;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void getBytesFromUUIDTest(){
        context.getBytesFromUUID(UUID.randomUUID());
    }
    @Test
    public void createLocalPassportTest(){
        context.createLocalPassport("Test","test");
    }
    @Test
    public void createRemotePassportTest(){
        DSRPassport remotePassport = new DSRPassport();
        remotePassport.setPassport(new DSRPassport());
        context.createRemotePassport(remotePassport);
    }

    @Test
    public void createPassportHexStringTest(){
        context.createPassportHexString(dsrPassport);
    }

    @Test(expected =Exception.class)
    public void readPassportFromHexStringTest(){
        context.readPassportFromHexString("dsrPassport");
    }

    @Test
    public void readPassportFromHexStringNull(){
        context.readPassportFromHexString(null);
    }

    @Test
    public void readPassportFromHexStringNullTest(){
        context.readPassportFromHexString(null);
    }
}
