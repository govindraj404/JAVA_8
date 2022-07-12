package com.sap.ic.cmh.masterdata.address.handler;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.address.validations.AddressValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;

import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Addresses;

public class AddressHandlerTest {

	@InjectMocks
	@Autowired
	public AddressHandler handler = new AddressHandler();
	@Mock
	public AddressValidator addressValidator;
	@Mock
	public Messages messages;
	@Mock
	public AuditLogHelper auditLogHelper;

	@Mock
	Result result;
	@Mock
	CdsCreateEventContext createContextMock;
    @Mock
	CdsUpdateEventContext updateEventContext;
	@Mock
	CqnInsert cqnInsert;

    @Mock 
    public AuditLogDifference<Addresses> auditLogDifference;

    @Mock
    public AddressService addressService;
	@Mock
	private CdsService cdsService;

    
	private Addresses addressItem;

	public AddressHandlerTest() {

		super();
	}

	public static final Logger logger = LoggerFactory.getLogger(AddressHandlerTest.class);

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
        addressItem.setId("123");

		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void testInsertAddress() {
		String addressId = UUID.randomUUID().toString();
		Addresses addressItem = Mockito.mock(Addresses.class);
		LoggerHelper log = Mockito.mock(LoggerHelper.class);
		addressItem.setId(addressId);
		addressItem.setAddress("test");
	//	CdsCreateEventContext createContextMock = Mockito.mock(CdsCreateEventContext.class);
		log.logMethodEntry(logger, "test", "test");
		doNothing().when(addressValidator).checkInputsSanitized(addressItem);
		//doNothing().when(exceptionUtility).raiseExceptionIfError(addressId, null, null);
		handler.beforeAddressCreate(createContextMock, addressItem);
	}
	@Test
	public void testOnCreateAddressNull() {
		when(addressService.getAddressDetailsBasedOnAddress(addressItem.getAddress())).thenReturn(null);
		handler.onAddressCreate(createContextMock, addressItem);
	}

	@Test
	public void testOnCreateAddressNotNull() {
		when(addressService.getAddressDetailsBasedOnAddress(addressItem.getAddress())).thenReturn(addressItem);
		handler.onAddressCreate(createContextMock, addressItem);
	}

    @Test
    public void testLogUpsert() {
        when(addressService.getAddress(anyString())).thenReturn(Addresses.create());
        Addresses mockedObj = Addresses.create();
        mockedObj.setId("test-id");
        assertDoesNotThrow(() -> handler.logUpsert(Action.CREATE, mockedObj));
    }

    @Test
    public void testSetOldAuditData() {
        Addresses addresses = Addresses.create();
        addresses.setId("123");

        Addresses mockAction = mock(Addresses.class);
        when(addressService.getAddress(anyString())).thenReturn(mockAction);

        assertDoesNotThrow(
                () -> handler.setOldAuditData(addresses));
    }
    
    @Test
    public void testAfterAddressCreateUpdate(){
        when(addressService.getAddress(addressItem.getId())).thenReturn(addressItem);
         assertDoesNotThrow(
                () -> handler.afterAddressCreateUpdate(addressItem));
    }

        @Test
    public void testBeforeAddressUpdate(){
		String addressId = UUID.randomUUID().toString();
		Addresses addressItem = Mockito.mock(Addresses.class);
		addressItem.setId(addressId);
		addressItem.setAddress("test");
		doNothing().when(addressValidator).checkInputsSanitized(addressItem);
        when(addressService.getAddress(addressItem.getId())).thenReturn(addressItem);
         assertDoesNotThrow(
                () -> handler.beforeAddressUpdate(updateEventContext,addressItem));
    }


}
