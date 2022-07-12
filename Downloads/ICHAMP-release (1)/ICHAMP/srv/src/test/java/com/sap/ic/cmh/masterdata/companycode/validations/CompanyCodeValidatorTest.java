package com.sap.ic.cmh.masterdata.companycode.validations;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
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

import java.util.*;

import static org.mockito.Mockito.when;

public class CompanyCodeValidatorTest {
    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private Messages messages;
    @Mock
    private Message msg;

    @Mock
    private Runnable run;

    @InjectMocks
    private CompanyCodeValidatorImpl validator;
    private CompanyCodes companyCodeItem;
    private Addresses addressItem;
    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        companyCodeItem = Struct.create(CompanyCodes.class);

        companyCodeItem.setAddress("10000001");
        companyCodeItem.setCompanyCode("1001");
        companyCodeItem.setCompanyCodeName("Test CC");
        companyCodeItem.setCountryKeyCode("DE");
        companyCodeItem.setCurrencyCode("EUR");

        addressItem = Struct.create(Addresses.class);
        addressItem.setId(UUID.randomUUID().toString());
        addressItem.setAddress(companyCodeItem.getAddress());

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }
    @Test
    public void checkInputsSanitized_ValidationPass() {

        Assertions.assertDoesNotThrow(() -> validator.checkInputsSanitized(companyCodeItem));
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericData() {
        companyCodeItem.setCompanyCode("<html><head></head><body></body></html>");
        Mockito.when(messages.error("any","test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        validator.checkInputsSanitized(companyCodeItem);
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaData() {
        companyCodeItem.setCompanyCodeName("<h45*@");
        //jupitar api junit test void method mocking
        Mockito.doNothing().when(dataValidator).validateAlphabeticData("", "test", CompanyCodes_.class, CompanyCodes_::companyCode, true,true);
        Mockito.when(messages.error("any","test")).thenReturn(msg);
        Mockito.when(msg.target("any")).thenReturn(msg);
        validator.checkInputsSanitized(companyCodeItem);
    }

    @Test
    public void checkInputsSanitized_ValidationateData() {
        companyCodeItem.setCurrencyCode("Henry-Strasse");
        validator.checkInputsSanitized(companyCodeItem);
    }

    @Test
    public void checkInputsSanitized_validateCountry() {
        Map<String, String> countryKey=new HashMap<>();
        countryKey.put("Henry-Strasse","test");
        companyCodeItem.setCountryKey(countryKey);
        validator.checkInputsSanitized(companyCodeItem);
    }
}
