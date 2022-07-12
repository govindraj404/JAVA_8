package com.sap.ic.cmh.masterdata.address.service;


import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.Addresses_;
import cds.gen.qualitynotificationservice.QualityNotifications;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.masterdata.address.model.AddressRequest;
import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import com.sap.ic.cmh.masterdata.address.repository.AddressRepositoryImpl;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AddressServiceImplTest {

    @Mock
    private AddressRepositoryImpl addressRepository;
    @InjectMocks
    @Autowired
    private AddressServiceImpl services;
    @Mock
    LocaleMessageHelper messageHelper;

    @Mock
    private AuditLogHelper auditLogHelper;
    @Mock
    protected PersistenceService db;

    @Mock
    private Messages messages;
    @Mock
    private Message message;
    @Mock
    Result result;
    @Mock
    Row row;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CqnInsert cqnInsert;
    private AddressRequest addressItem;
    private Addresses address;
    List<AddressRequest> list = new ArrayList<AddressRequest>();
    List<AddressResponse> addressResponseList = new ArrayList<>();


    private static Map<String, String> addressPartnerDb;
    private static List<String> addresskeys;
    private static Map<String, String> addressPartnerMap;
    private static List<AddressRequest> addressLists;


    private static String id = null;

    @Before
    public void beforeClass() throws JsonProcessingException {
        MockitoAnnotations.openMocks(this);
        address = Struct.create(Addresses.class);

        address.setAddress("1");
        address.setStreet("Henry-Strasse");
        address.setHouseNumber("70839");
        address.setAddressLine1("D52078");
        address.setAddressLine2("D52078");
        address.setAddressLine2("D52038");
        address.setPostalCode("85386");
        address.setCity("Koeln");
        address.setCountryKeyCode("DE");
        address.setCountry("Germany");
        address.setPoBox("5226");
        address.setMobile("+91 1234567890");
        address.setTelephone("+91 1234567890");
        address.setExtension("13333");
        address.setFaxNumber("+91 1234567890");
        address.setEmail("witich.klein@gmail.com");
        address.setContactPerson("Dr Witich Klein");

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
        addressItem=new AddressRequest();
        addressItem.setAddress(address.getAddress());
        //addressItem.set
        list.add(addressItem);

        id =UUID.randomUUID().toString();

        addresskeys =new ArrayList<>();
        addresskeys.add("10000001");

        addressPartnerMap =new HashMap<>();
        addressPartnerMap.put("10000001","4052c63c-6351-4ea5-8192-4af6686bb526");

        String json = "[{\"address\":\"10000001\"}]";
        addressLists =new ObjectMapper().readValue(json,
                new TypeReference<List<AddressRequest>>() {
                });
    }

    @Test
    public  void deleteAddressListTest() {
        Map<String, String> addressDb = new HashMap<String, String>();
        List<String> stringList=new ArrayList<>();
        when(addressRepository.getAddressMap(stringList)).thenReturn(addressDb) ;
        when(messageHelper.getMessage("test")).thenReturn("test");
        List<AddressResponse> addressResponses = services.deleteAddressList(list);
        Assert.assertNotNull(addressResponses.toString());
        Assert.assertEquals("1", addressResponses.get(0).getAddress());
    }
    @Test
    public void testDeleteAddressCode() {
        Optional<Addresses> emptyOpt = Optional.empty();
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Addresses.class)).thenReturn(emptyOpt);
        services.deleteAddressList(list);
    }

    @Test
    public void testDeleteAddressNotEmpty() {
        List<String> addressKeys = new ArrayList<>();
        addressKeys.add("test");
        Map<String, String> addressDb = new HashMap<>();
        addressDb.put(address.getAddress(),"test");
        addressDb.put(address.getAddress(),"test2");
        when(addressRepository.getAddressMap(addressKeys)).thenReturn(addressDb);
        Optional<Addresses> emptyOpt = Optional.of(address);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Addresses.class)).thenReturn(emptyOpt);
        services.deleteAddressList(list);
    }



    @Test
   public void deleteAddressListsTest() {
        addressPartnerDb = new HashMap<>();
        addressPartnerDb.put(id, "4052c63c-6351-4ea5-8192-4af6686bb526");
        Mockito.when(addressRepository.getAddressMap(addresskeys)).thenReturn(addressPartnerDb);
        List<String> businessPartnerList;
        businessPartnerList = new ArrayList<>(addressPartnerDb.keySet());
        businessPartnerList.add("10000001");
        List<AddressResponse> addressResponses = services.deleteAddressList(addressLists);
        //Assertions.assertEquals(businessPartnerResponses.size(), 0);
    }

    @Test
    public void testGetAddress() {
        Result result = mock(Result.class);
        when(addressRepository.getAddressDetails(any(String.class))).thenReturn(result);
        services.getAddress("122");
    }

    @Test
    public void testGetActionPrecondition() {
        Result result = mock(Result.class);
        when(addressRepository.getAddressDetailsBasedOnAddress(any(String.class))).thenReturn(result);
        services.getAddressDetailsBasedOnAddress("result");
    }

	@Test
	public void testFetchAddress() {
		Optional<Addresses> opt = Optional.empty();
		when(addressRepository.fetchAddress(address.getId(), "test", Addresses_.class, Addresses_::address))
				.thenReturn(result);
		 Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
	        Mockito.when(message.target(any(String.class))).thenReturn(message);
		when(result.first(Addresses.class)).thenReturn(opt);
		services.fetchAddress(address.getId(), "test", Addresses_.class, Addresses_::address);
	}
	
//	@Test
//	public void testFetchAddressNotNull() {
//		Optional<Addresses> opt = Optional.of(address);
//		when(addressRepository.fetchAddress(address.getId(), "test", Addresses_.class, Addresses_::address))
//				.thenReturn(result);
//		when(result.first(Addresses.class)).thenReturn(opt);
//		
//		services.fetchAddress(address.getId(), "test", Addresses_.class, Addresses_::address);
//	}
//    
//    
//    @Test
//    public void testGetAddressDetailsNull() {
//    	List<Row> rowvalues = new ArrayList<>();
//        row.put("address", "ComplaintID");
//        row.put("ID", "1");
//        Optional<Row> optRow = Optional.empty();
//        List<Addresses> addressList = new ArrayList<>();
//        rowvalues.add(row);
//        Mockito.when(result.list()).thenReturn(rowvalues);
//        Mockito.when(result.first()).thenReturn(optRow);
//        Mockito.when(result.listOf(Addresses.class)).thenReturn(addressList);
//        when(addressRepository.getAddressDetails(address.getId())).thenReturn(result);
//    	services.getAddressDetailsBasedOnAddress(address.getAddress());
//    }
//    
//    @Test
//    public void testGetAddressDetailsNotNull() {
//    	 List<Row> rowvalues = new ArrayList<>();
//         row.put("address", "ComplaintID");
//         row.put("ID", "1");
//         Optional<Row> optRow = Optional.of(row);
//         List<Addresses> addressList = new ArrayList<>();
//         addressList.add(address);
//         rowvalues.add(row);
//         Mockito.when(result.list()).thenReturn(rowvalues);
//         Mockito.when(result.first()).thenReturn(optRow);
//         Mockito.when(result.listOf(Addresses.class)).thenReturn(addressList);
//    	when(addressRepository.getAddressDetails(address.getId())).thenReturn(result);
//    	services.getAddressDetailsBasedOnAddress(address.getAddress());
//    }



}

