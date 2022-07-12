package com.sap.ic.cmh.masterdata.companycode.handler;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.companycode.validations.CompanyCodeValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CompanyCodeHandlerTest {
    @InjectMocks
    @Autowired
    private CompanyCodeHandler handler;
    @Mock
    protected PersistenceService mockDb;
    @Mock
    AddressService addressService;
    @Mock
    private Messages messages;
    @Mock
    private CompanyCodeValidator companyCodeValidator;

    @Mock
    Result result;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CdsUpdateEventContext context;
    @Mock
    CompanyCodeService service;
    @Mock
    CdsService cdsService;
    @Mock
    CqnInsert cqnInsert;
    

    private CompanyCodes companyCodeItem;
    private Addresses addressItem;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        companyCodeItem = Struct.create(CompanyCodes.class);

        companyCodeItem.setAddress("10000001");
        companyCodeItem.setCompanyCode("1001");
        companyCodeItem.setCompanyCodeName("Test CC");
        companyCodeItem.setCountryKeyCode("DE");
        companyCodeItem.setCurrencyCode("EUR");
        companyCodeItem.setAddressIDId("12");
        addressItem = Struct.create(Addresses.class);
        addressItem.setId(UUID.randomUUID().toString());
        addressItem.setAddress(companyCodeItem.getAddress());

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void testCreateCompanyCode() {
        Optional<CompanyCodes> emptyOpt = Optional.empty();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(emptyOpt);
        when(service.fetchCompanyCodesBasedOnCode(companyCodeItem.getCompanyCode())).thenReturn(companyCodeItem);
        handler.onCompanyCodeCreate(createContextMock, companyCodeItem);
    }

    @Test
    public void testUpdateCompanyCode() {
        Optional<CompanyCodes> opt = Optional.of(companyCodeItem);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);

        handler.onCompanyCodeCreate(createContextMock, companyCodeItem);
    }

    @Test
    public void testUpdateCompanyCode_addressNotNull() {
        Optional<CompanyCodes> opt = Optional.of(companyCodeItem);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        when(addressService.fetchAddress(any(), any(), any(), any())).thenReturn(addressItem);
        handler.onCompanyCodeCreate(createContextMock, companyCodeItem);
    }

    @Test
    public void TestBeforeCompanyCodeCreate(){
        Optional<CompanyCodes> opt = Optional.of(companyCodeItem);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(CompanyCodes.class)).thenReturn(opt);
        when(addressService.fetchAddress(companyCodeItem.getAddress(),"ADDRESS_DOES_NOT_EXIST", CompanyCodes_.class, CompanyCodes_::address)).thenReturn(addressItem);
        handler.beforeCompanyCodeCreate(createContextMock,companyCodeItem);
    }


    @Test
    public void TestBeforeCompanyCodeUpdate(){
        handler.beforeCompanyCodeUpdate(context,companyCodeItem);
    }

}