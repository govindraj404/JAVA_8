package com.sap.ic.cmh.pdm;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.context.MessageSource;

import java.io.InputStream;
import java.util.Locale;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class AnnotationControllerTest {
    @InjectMocks
    AnnotationController client;
    @Mock
    MessageSource source;
    @Mock
    Locale locale;
    @Mock
    InputStream inputStream;
    @Mock
    MessageSource messageSource;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    
    // @Test(expected = Exception.class)
    // public void getBusinessPartnerMetadataTest() throws PDMException {
    //     client.getBusinessPartnerMetadata(locale);
    // }
    // @Test(expected = Exception.class)
    // public void getBusinessPartnerMetadataElseTest() throws PDMException {
    //     String msg="messageKey-not-found";
    //     when(source.getMessage(any(String.class), any(Object[].class), any(String.class), any(Locale.class))).thenReturn(msg);
    //     client.getBusinessPartnerMetadata(locale);
    // }


    // @Test
    // public void getBtpUserMetadataTest() throws PDMException {

    //     client.getTextBundle("security/pdm/pdmBusinessPartnerAnnotations.json");
    // }
    @Test
    public void safeCloseTest() {

        client.safeClose(inputStream);
    }

    @Test
    public void setMessageSourceTest()  {

        client.setMessageSource(messageSource);
    }
}
