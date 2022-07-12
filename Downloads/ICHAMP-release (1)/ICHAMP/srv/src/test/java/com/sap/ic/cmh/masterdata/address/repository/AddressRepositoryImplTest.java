package com.sap.ic.cmh.masterdata.address.repository;


import cds.gen.masterdataservice.*;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class AddressRepositoryImplTest {
    @InjectMocks
    AddressRepositoryImpl repository;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Messages messages;
    @Mock
    Message message;

    private final List<String> addressCodes = new ArrayList<>();
    private final List<Addresses> addressCodesList = new ArrayList<>();
    private final List<Countries> countriesList = new ArrayList<>();
    private final List<Plants> plantList = new ArrayList<>();
    private final List<BusinessPartners> BusinessPartnersList = new ArrayList<>();
    private Addresses address;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        addressCodes.add("001");

        address = (Addresses) Struct.create(Addresses.class);
        address.setId("100");
        address.setAddress("F001");
        addressCodesList.add(address);

        Countries countries = Struct.create(Countries.class);
        countries.setCode("100");
        countriesList.add(countries);

        Plants plants = Struct.create(Plants.class);
        plants.setAddressIDId("100");
        plants.setAddress(address.getAddress());
        plants.setId("001");
        plantList.add(plants);

        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setAddress(address.getAddress());
        businessPartners.setAddressIDId(address.getId());
        BusinessPartnersList.add(businessPartners);

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }

    @Test
    public void testGetAddressMap() {
        Map<String, String> companyCodeMap = new HashMap<>();
        companyCodeMap.put("F001","100");
        when(result.listOf(Addresses.class)).thenReturn(addressCodesList);

        assertEquals(companyCodeMap, repository.getAddressMap(addressCodes));
    }

    @Test
    public void testGetActiveCountriesInAddressCode() {
        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Countries.class)).thenReturn(countriesList);

        repository.getCompanyCodeInAddress(addressCodes);
    }

    @Test
    public void testDeleteAddressList() {
        long deleteCount = 0;
        when(result.rowCount()).thenReturn(deleteCount);

        repository.deleteAddressList(addressCodes);
    }


    @Test
    public void testGetPlantMap() {
        Map<String, String> plantMap = new HashMap<>();
        plantMap.put("F001","100");
        when(result.listOf(Plants.class)).thenReturn(plantList);

        repository.getPlantDetails(addressCodes);
    }

    @Test
    public void testGetBusinessPartner() {
        Map<String, String> businessPartnerMap = new HashMap<>();
        businessPartnerMap.put("F001","100");
        when(result.listOf(BusinessPartners.class)).thenReturn(BusinessPartnersList);

        repository.getBusinessPartner(addressCodes);
    }
    
    @Test
    public void getAddressTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        repository.fetchAddress(address.getId(), "test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void getAddressNullTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        address.setId(null);
        repository.fetchAddress(address.getId(),"test", Addresses_.class, Addresses_::address);
    }


    @Test
    public void getAddressEmptyTest(){
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
        address.setId("");
        repository.fetchAddress(address.getId(),"test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void testGetAddress() {
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        Mockito.when(message.target(any(String.class))).thenReturn(message);
       // Optional<Addresses> opt = Optional.empty();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
      //  when(result.first(Addresses.class)).thenReturn(opt);
        repository.fetchAddress(address.getId(),"test", Addresses_.class, Addresses_::address);
    }

    @Test
    public void testUpdateAddressCode() {
     //   Optional<Addresses> opt = Optional.of(address);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
     //   when(result.first(Addresses.class)).thenReturn(opt);
        repository.fetchAddress(address.getId(),"test", Addresses_.class, Addresses_::address);
    }
    
    @Test
    public void testGetAddressDetails() {
    	 when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
    	 repository.getAddressDetails(address.getId());
    }


    @Test
    public void testGetAddressDetailsBasedOnAddress() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        repository.getAddressDetailsBasedOnAddress(address.getId());
    }
}
