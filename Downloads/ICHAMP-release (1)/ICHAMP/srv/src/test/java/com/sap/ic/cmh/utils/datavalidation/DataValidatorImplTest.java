package com.sap.ic.cmh.utils.datavalidation;

import cds.gen.masterdataservice.Addresses_;
import cds.gen.masterdataservice.DefectCodes_;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.SecurityValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.any;

public class DataValidatorImplTest {
    @InjectMocks
    DataValidatorImpl impl;
    @Mock
    private SecurityValidator securityValidator;
    @Mock
    private MasterDataDao masterDataDao;
    @Mock
    Messages messages;
    @Mock
    Message message;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        //eightD = Struct.create(QualityNotifications.class);
    }
    @Test
    public void validateEmailTest(){
        impl.validateEmail("syyed@gmial.com","test", Addresses_.class, Addresses_::email,true);
    }
    @Test
    public void validateEmailNullTest(){

        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateEmail(null,"test", Addresses_.class, Addresses_::email,true);
    }
    @Test
    public void validateEmailWithRunTest(){
        impl.validateEmail("syed@gmail.com","test", Addresses_.class, Addresses_::email);
    }
    @Test
    public void validateEmailWithRunNullTest(){
        impl.validateEmail(null,"test", Addresses_.class, Addresses_::email);
    }
    @Test
    public void validateMobileTest(){

        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobile("849548549","test", Addresses_.class, Addresses_::mobile);
    }
    @Test
    public void validateMobileNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobile("","test", Addresses_.class, Addresses_::mobile);
    }

    @Test
    public void validateMobileWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobile("849548549","test", Addresses_.class, Addresses_::mobile,true);
    }
    @Test
    public void validateMobileWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobile("","test", Addresses_.class, Addresses_::mobile,true);
    }
    @Test
    public void validateNumberTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobile("849548549","test", Addresses_.class, Addresses_::faxNumber);
    }
    @Test
    public void validateNumberNullTest(){
        impl.validateNumber("","test", Addresses_.class, Addresses_::faxNumber);
    }

    @Test
    public void validateNumberWithRunTest(){
        impl.validateNumber("849548549","test", Addresses_.class, Addresses_::faxNumber,true);
    }
    @Test
    public void validateNumberWithRunNullTest(){
        impl.validateNumber("","test", Addresses_.class, Addresses_::faxNumber,true);
    }
    @Test
    public void validateDataTest(){

        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateData("849548549","test", Addresses_.class, Addresses_::address);
    }
    @Test
    public void validateDataNullTest(){
        impl.validateData("","test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void validateDataWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateData("849548549","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateDataWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateData("","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateDataWithMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateData("849548549","test", Addresses_.class, Addresses_::address,true,true);
    }
    @Test
    public void validateDataWithRunNullMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateData("","test", Addresses_.class, Addresses_::address,true,true);
    }

    @Test
    public void validateAlphabeticDataTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphabeticData("849548549","test", Addresses_.class, Addresses_::address);
    }
    @Test
    public void validateAlphabeticDataNullTest(){
        impl.validateAlphabeticData("","test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void validateAlphabeticDataWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphabeticData("849548549","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateAlphabeticDataWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphabeticData("","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateAlphabeticDataWithMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphabeticData("849548549","test", Addresses_.class, Addresses_::address,true,true);
    }
    @Test
    public void validateAlphabeticDataWithRunNullMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphabeticData("","test", Addresses_.class, Addresses_::address,true,true);
    }


    @Test
    public void validateAlphaNumericDataTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericData("849548549","test", Addresses_.class, Addresses_::address);
    }
    @Test
    public void validateAlphaNumericDataNullTest(){
        impl.validateAlphaNumericData("","test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void validateAlphaNumericDataDataWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericData("849548549","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateAlphaNumericDataWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericData("","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateAlphaNumericDataWithMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericData("849548549","test", Addresses_.class, Addresses_::address,true,true);
    }
    @Test
    public void validateAlphaNumericDataWithRunNullMontyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericData("","test", Addresses_.class, Addresses_::address,true,true);
    }


    @Test
    public void validateCurrenciesTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCurrencies("849548549","test", Addresses_.class, Addresses_::address);
    }
    @Test
    public void validateCurrenciesNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCurrencies("","test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void validateCurrenciesWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCurrencies("849548549","test", Addresses_.class, Addresses_::address,true);
    }
    @Test
    public void validateCurrenciesWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCurrencies("","test", Addresses_.class, Addresses_::address,true);
    }

    @Test
    public void validateCountryTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCountry("849548549","test", Addresses_.class, Addresses_::country);
    }
    @Test
    public void validateCountryNullTest(){

        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCountry("","test", Addresses_.class, Addresses_::country);
    }

    @Test
    public void validateCountryWithRunTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCountry("849548549","test", Addresses_.class, Addresses_::country,true);
    }
    @Test
    public void validateCountryWithRunNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCountry("","test", Addresses_.class, Addresses_::country,true);
    }

    @Test
    public void testValidateEmailImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateEmailImpl("pawan@gmail.com", "test", Addresses_.class, Addresses_::country, true);
        impl.validateEmailImpl("pawan@gmail.com", "test", Addresses_.class, Addresses_::country, false);
        impl.validateEmailImpl("", "test", Addresses_.class, Addresses_::country, false);
        impl.validateEmailImpl("", "test", Addresses_.class, Addresses_::country, true);
    }
    @Test
    public void TestValidateMobileImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateMobileImpl("91-9935655220", "test", Addresses_.class, Addresses_::country, true);
        impl.validateMobileImpl(null, "test", Addresses_.class, Addresses_::country, true);
        impl.validateMobileImpl("91-9935655220", "test", Addresses_.class, Addresses_::country, false);
        impl.validateMobileImpl("", "test", Addresses_.class, Addresses_::country, false);
    }

    @Test
    public void testValidateNumberImplElse(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateNumberImpl(null, "test", Addresses_.class, Addresses_::country, true);

    }
    @Test
    public void testValidateNumberImplNull(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateNumberImpl("null", "test", Addresses_.class, Addresses_::country, true);

    }
    @Test
    public void testValidateNumberImplEmpty(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateNumberImpl("123", "test", Addresses_.class, Addresses_::country, true);

    }
    @Test
    public void testValidateNumberImplEmpty1(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateNumberImpl("test", "test", Addresses_.class, Addresses_::country, false);

    }

    @Test
    public void testValidateAlphaNumericDataImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericDataImpl("BZOPP1891E","test", Addresses_.class, Addresses_::country,true,true);
        impl.validateAlphaNumericDataImpl(null, "test", Addresses_.class, Addresses_::country, true, true);
        impl.validateAlphaNumericDataImpl("", "test", Addresses_.class, Addresses_::country, true, true);
        impl.validateAlphaNumericDataImpl("", "test", Addresses_.class, Addresses_::country, false, true);
        impl.validateAlphaNumericDataImpl("", "test", Addresses_.class, Addresses_::country, false, false);
        impl.validateAlphaNumericDataImpl(null, "test", Addresses_.class, Addresses_::country, false, false);
    }

    @Test
    public void testValidateAlphaNumericDataImplElse(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateAlphaNumericDataImpl("(*##)","test", Addresses_.class, Addresses_::country,true,true);

    }


    @Test
    public void testValidateCurrenciesImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCurrencies("INR","test", Addresses_.class, Addresses_::country,true);
        impl.validateCurrencies(null,"test", Addresses_.class, Addresses_::country,true);
        impl.validateCurrencies("","test", Addresses_.class, Addresses_::country,true);
    }

    @Test
    public void testValidateCountryImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateCountryImpl("INR","test", Addresses_.class, Addresses_::country,true);
        impl.validateCountryImpl("","test", Addresses_.class, Addresses_::country,true);
        impl.validateCountryImpl(null,"test", Addresses_.class, Addresses_::country,true);
    }

    @Test
    public void testValidateDataImpl(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateDataImpl("abc","test", Addresses_.class, Addresses_::country,true,true);
        impl.validateDataImpl("","test", Addresses_.class, Addresses_::country,true,true);
        impl.validateDataImpl(null,"test", Addresses_.class, Addresses_::country,true,true);
        impl.validateDataImpl("abc","test", Addresses_.class, Addresses_::country,true,false);
        impl.validateDataImpl("","test", Addresses_.class, Addresses_::country,true,false);
        impl.validateDataImpl(null,"test", Addresses_.class, Addresses_::country,true,false);
    }

    @Test
    public void testValidateDataWithSpecialChar(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateDataWithSpecialChar("testData$","test", DefectCodes_.class, DefectCodes_::code, true, true, Constants.DEFCODERESTRCHAR);
        impl.validateDataWithSpecialChar(null,"test", DefectCodes_.class, DefectCodes_::code, true, true, Constants.DEFCODERESTRCHAR);
    }

    @Test
    public void testWtVolUnitCode(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateDataWithSpecialChar("abcABC1230%","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);
        impl.validateDataWithSpecialChar("01234%","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);
        impl.validateDataWithSpecialChar(null,"test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);  // null check
        impl.validateDataWithSpecialChar("","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR);  // empty check
        impl.validateDataWithSpecialChar("abc@","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR); // false case
        impl.validateDataWithSpecialChar("123$","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.WTVOLUNITACCEPCHAR); // false case
    }

    @Test
    public void testBaseUnitCode(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        impl.validateDataWithSpecialChar("abcABC1230%/","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR);
        impl.validateDataWithSpecialChar("01234% ","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR);
        impl.validateDataWithSpecialChar(null,"test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR); // null check
        impl.validateDataWithSpecialChar("","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR); // empty check
        impl.validateDataWithSpecialChar("abc@","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR); // false case
        impl.validateDataWithSpecialChar("123$","test", MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::weightUnit_code,false,true,Constants.BASEUOMACCEPCHAR); // false case
    }
    
}
