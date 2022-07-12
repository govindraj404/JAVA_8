package com.sap.ic.cmh.masterdata.businesspartner.handler;

import cds.gen.masterdataservice.*;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.businesspartner.validations.BusinessPartnerValidator;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;


public class BusinessPartnerHandlerTest {

    @InjectMocks
    @Autowired
    private BusinessPartnerHandler handler;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    AuditLogHelper auditLogHelper;

    @Mock
    private BusinessPartnerValidator businessPartnerValidator;

    @Mock
    private AddressService addressService;

    @Mock
	CompanyCodeService companyCodeService;

    @Mock
    Messages messages;
    @Mock
    private Message msg;

    private BusinessPartners businessPartnerItem;

    private BusinessPartners businessPartnerItem2;

    private BusinessPartners businessPartnerItem3;

    private BusinessPartners businessPartnerItem4;

    private CompanyCodes companyCodeItem;
    @Mock
    private CdsService cdsService;

    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CdsUpdateEventContext context;

    @Mock
    CqnInsert cqnInsert;

    @Mock
    Result result;
    @Mock
    BusinessPartnerService service;

    private static Addresses addressItem;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        businessPartnerItem = Struct.create(BusinessPartners.class);

        businessPartnerItem.setBusinessPartnerNumber("10000001");
        businessPartnerItem.setVendorCode("1100000001");
        businessPartnerItem.setCustomerCode("2100000001");
        businessPartnerItem.setCompanyCode("BP01");
        businessPartnerItem.setBusinessPartnerName1("Auto Werke GmbH");
        businessPartnerItem.setBusinessPartnerName2("Robert Bosch GmbH");
        businessPartnerItem.setAddress("10000001");
        businessPartnerItem.setBusinessPartnerType("SUP");


        businessPartnerItem2 = Struct.create(BusinessPartners.class);

        businessPartnerItem2.setBusinessPartnerNumber("10000007");
        businessPartnerItem2.setVendorCode("1100000001");
        businessPartnerItem2.setCustomerCode(null);
        businessPartnerItem2.setCompanyCode("BP01");
        businessPartnerItem2.setBusinessPartnerName1("Auto Werke GmbH");
        businessPartnerItem2.setBusinessPartnerName2("Robert Bosch GmbH");
        businessPartnerItem2.setAddress("10000001");
        businessPartnerItem2.setBusinessPartnerType("SUP");

        businessPartnerItem3 = Struct.create(BusinessPartners.class);

        businessPartnerItem3.setBusinessPartnerNumber("10000007");
        businessPartnerItem3.setVendorCode(null);
        businessPartnerItem3.setCustomerCode("19999050");
        businessPartnerItem3.setCompanyCode("BP01");
        businessPartnerItem3.setBusinessPartnerName1("Auto Werke GmbH");
        businessPartnerItem3.setBusinessPartnerName2("Robert Bosch GmbH");
        businessPartnerItem3.setAddress("10000001");
        businessPartnerItem3.setBusinessPartnerType("SUP");

        businessPartnerItem4 = Struct.create(BusinessPartners.class);

        businessPartnerItem4.setBusinessPartnerNumber("10000007");
        businessPartnerItem4.setVendorCode(null);
        businessPartnerItem4.setCustomerCode(null);
        businessPartnerItem4.setCompanyCode("BP01");
        businessPartnerItem4.setBusinessPartnerName1("Auto Werke GmbH");
        businessPartnerItem4.setBusinessPartnerName2("Robert Bosch GmbH");
        businessPartnerItem4.setAddress("10000001");
        businessPartnerItem4.setBusinessPartnerType("SUP");

        addressItem = Struct.create(Addresses.class);
        addressItem.setId(UUID.randomUUID().toString());
        addressItem.setAddress(businessPartnerItem.getAddress());


        companyCodeItem = Struct.create(CompanyCodes.class);
        companyCodeItem.setId(UUID.randomUUID().toString());
        companyCodeItem.setCompanyCode(businessPartnerItem.getCompanyCode());

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void testCreateBusinessPartner()  {
        when(service.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber())).thenReturn(businessPartnerItem);
        handler.onBusinessPartnersCreate(createContextMock, businessPartnerItem);
    }

    @Test
    public void testUpdateBusinessPartner() {
        Mockito.when(service.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber())).thenReturn(businessPartnerItem);
        handler.onBusinessPartnersCreate(createContextMock, businessPartnerItem);
    }

    @Test
    public void testBusinessPartnerAddressNotNull() {
        Mockito.when(service.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber())).thenReturn(null);
        handler.onBusinessPartnersCreate(createContextMock, businessPartnerItem);
    }

    @Test
    public void testBusinessPartnerCustomerCodeNull() {
        Mockito.when(addressService.fetchAddress(any(), any(), any(), any())).thenReturn(null);
        Mockito.when(companyCodeService.fetchCompanyCode(any(), any(), any(), any())).thenReturn(null);
        handler.beforeBusinessPartnersUpdate(context, businessPartnerItem2);
    }
    @Test
    public void testBusinessPartnerVendorCodeNull() {
        Optional<BusinessPartners> opt = Optional.of(businessPartnerItem3);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.first(BusinessPartners.class)).thenReturn(opt);
        handler.beforeBusinessPartnersCreate(createContextMock, businessPartnerItem3);
    }

    @Test
    public void testBusinessPartnerVendorCodeAndCustomerCodeNull() {
        Optional<BusinessPartners> opt = Optional.of(businessPartnerItem4);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.first(BusinessPartners.class)).thenReturn(opt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        handler.beforeBusinessPartnersCreate(createContextMock, businessPartnerItem4);

    }


    @Test
    public void testBusinessPartnerCompanyCodeNotNull() {
        Optional<BusinessPartners> opt = Optional.of(businessPartnerItem);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.first(BusinessPartners.class)).thenReturn(opt);
        Mockito.when(companyCodeService.fetchCompanyCode(any(), any(), any(), any())).thenReturn(companyCodeItem);
        handler.beforeBusinessPartnersCreate(createContextMock, businessPartnerItem);
    }

    @Test
    public void testBusinessPartnerElse() {
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        Optional<BusinessPartners> opt = Optional.empty();
        when(addressService.fetchAddress(any(), any(), any(), any())).thenReturn(addressItem);
        Mockito.when(companyCodeService.fetchCompanyCode(any(), any(), any(), any())).thenReturn(companyCodeItem);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        handler.beforeBusinessPartnersCreate(createContextMock, businessPartners);
    }

    @Test
    public void afterBusinessPartnerCreateUpdateTest() {
        BusinessPartners addresse=Struct.create(BusinessPartners.class);
        addresse.setId("78");
        List<BusinessPartners> opt = new ArrayList<>();
        opt.add(addresse);
        //handler.afterBusinessPartnerCreateUpdate(opt);
    }
    @Test
    public void afterBusinessPartnerDeletionTest() {
        BusinessPartners addresse=Struct.create(BusinessPartners.class);
        addresse.setId("78");
        //handler.afterBusinessPartnerDeletion(addresse);
    }

    @Test
    public void validateCustomerAndVendorTest() {
        BusinessPartners businessPartners=Struct.create(BusinessPartners.class);
        businessPartners.setVendorCode("106");
        businessPartners.setCustomerCode("201");
        Optional<BusinessPartners> opt = Optional.of(businessPartners);
        Optional<BusinessPartners> opt2 = Optional.empty();
        List<BusinessPartners> list=new ArrayList<>();
        list.add(businessPartners);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(list);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("itemType_code", "202");
        Optional<Row> op1 = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(op1);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(result.first(BusinessPartners.class)).thenReturn(opt);
        Mockito.when(messages.error(any(String.class),any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        handler.validateCustomerAndVendor(businessPartners,businessPartners);
    }
    
    @Test
    public void testBusinessPartnerCompanyCodeNotNullUpdate() {
        when(addressService.fetchAddress(any(), any(), any(), any())).thenReturn(addressItem);
        Mockito.when(companyCodeService.fetchCompanyCode(any(), any(), any(), any())).thenReturn(companyCodeItem);
        handler.beforeBusinessPartnersUpdate(context, businessPartnerItem);
    }

   
}