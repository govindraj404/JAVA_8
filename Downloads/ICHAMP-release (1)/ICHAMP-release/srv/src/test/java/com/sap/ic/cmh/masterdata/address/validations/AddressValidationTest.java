package com.sap.ic.cmh.masterdata.address.validations;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.Addresses_;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.exceptions.base.MockitoException;

import java.util.HashMap;
import java.util.Map;

public class AddressValidationTest {

    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;

    @InjectMocks
    private AddressValidatorImpl validator;
    private Addresses addressItem;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        addressItem = Struct.create(Addresses.class);
        addressItem.setAddress("10000001");
        addressItem.setStreet("Henry-Strasse");
        addressItem.setHouseNumber("70839");
        addressItem.setAddressLine1("D52078");
        addressItem.setAddressLine2("D52078");
        addressItem.setAddressLine2("D52038");
        addressItem.setPostalCode("85386");
        addressItem.setCity("Koeln");
        addressItem.setCountryKeyCode("DE");
        addressItem.setCountry("Germany");
        addressItem.setPoBox("5226");
        addressItem.setMobile("+91 1234567890");
        addressItem.setTelephone("+91 1234567890");
        addressItem.setExtension("13333");
        addressItem.setFaxNumber("+91 1234567890");
        addressItem.setEmail("witich.klein@gmail.com");
        addressItem.setContactPerson("Dr Witich Klein");

    }

    @Test
    public void checkInputsSanitized_ValidationPass() {

        Assertions.assertDoesNotThrow(() -> validator.checkInputsSanitized(addressItem));
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericData() {
        addressItem.setAddress("<html><head></head><body></body></html>");
        Mockito.when(messages.error("any", "test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        validator.checkInputsSanitized(addressItem);
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaData() {
        addressItem.setContactPerson("<h45*@");
        //jupitar api junit test void method mocking
        Mockito.doNothing().when(dataValidator).validateAlphabeticData("", "test", Addresses_.class, Addresses_::address, true,true);
        Mockito.when(messages.error("any","test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        validator.checkInputsSanitized(addressItem);
    }

    @Test
    public void checkInputsSanitized_ValidationateData() {
        addressItem.setStreet("Henry-Strasse");
        validator.checkInputsSanitized(addressItem);
    }

    @Test
    public void checkInputsSanitized_validateCountry() {
        Map<String, String> countryKey=new HashMap<>();
        countryKey.put("Henry-Strasse","test");
        addressItem.setCountryKey(countryKey);
        validator.checkInputsSanitized(addressItem);
    }

    @Test
    public void checkInputsSanitized_ValidationData() {
        addressItem.setEmail("<h45*");
        //jupitar api junit test void method mocking
        Mockito.doThrow(new MockitoException("fail")).doNothing().when(dataValidator).validateEmail(addressItem.getEmail(),"test", Addresses_.class, Addresses_::email,true);
        Mockito.when(messages.error("any","test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        validator.checkInputsSanitized(addressItem);
    }
}
