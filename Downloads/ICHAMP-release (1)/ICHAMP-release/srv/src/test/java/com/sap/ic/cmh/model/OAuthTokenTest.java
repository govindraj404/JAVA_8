package com.sap.ic.cmh.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class OAuthTokenTest {
    @InjectMocks
    OAuthToken oAuthToken;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void setMethodTest() {
        oAuthToken.setAccessToken("test");
        oAuthToken.setTokenType("test");
        oAuthToken.setExpiresIn("test");

    }
    @Test
    public void getMethodTest() {
        oAuthToken.getAccessToken();
        oAuthToken.getTokenType();
        oAuthToken.getExpiresIn();

    }
}
