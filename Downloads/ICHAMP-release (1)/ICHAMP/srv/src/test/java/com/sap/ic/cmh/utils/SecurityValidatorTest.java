package com.sap.ic.cmh.utils;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.safety.Whitelist;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class SecurityValidatorTest {
    @InjectMocks
    SecurityValidator securityValidator;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void validateTextTest(){
        securityValidator.isValidText("syyed");
    }

    @Test
    public void validateTextNullTest(){
        securityValidator.isValidText(null);
    }

    @Test
    public void validateTextEmptyTest(){
        securityValidator.isValidText("");
    }

    @Test
    public void validateTextValidValueTest(){
        String sTextToBeValidated="";
         boolean validValue = true;
        Document.OutputSettings settings = new Document.OutputSettings();
        String cleanedValue = Jsoup.clean(sTextToBeValidated, "", Whitelist.none(), settings);
          sTextToBeValidated.matches("^[a-zA-Z]*[0-9]*<[^<>]+>.*$");
       // validValue=sTextToBeValidated.matches("^[a-zA-Z]*[0-9]*<[^<>]+>.*$");
        securityValidator.isValidText( "pawan" );
    }

    @Test
    public void validateTextValidValueElseTest(){
        securityValidator.isValidText( "test123<1>.@$" );
    }

}
