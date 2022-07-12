package com.sap.ic.cmh.masterdata.plant.handler;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.Plants;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import com.sap.ic.cmh.masterdata.plant.validation.PlantValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class PlantHandlerTest {

    @InjectMocks
    @Autowired
    private PlantHandler plantHandler;

    @Mock
    protected PersistenceService mockDb;
    
    @Mock
    AddressService addressService;

    @Mock
	CompanyCodeService companyCodeService;

    @Mock
    private Messages messages;

    @Mock
    private PlantValidator plantValidator;
    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    PlantService plantService;

    @Mock
    CqnInsert cqnInsert;

    @Mock
    CdsUpdateEventContext updateEventContext;

    @Mock
    Result result;

    private Plants plantItem;

    private CompanyCodes companyCodeItem;

    private  Addresses addressItem;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        plantItem = Struct.create(Plants.class);

        plantItem.setPlant("BP15");
        plantItem.setPlantName("SPL Chemical");
        plantItem.setPlantNameExtension("Plant");
        plantItem.setCompanyCode("BP02");
        plantItem.setCustomerNoOfPlant("100015");
        plantItem.setSupplierNoOfPlant("WERK4500");
        plantItem.setAddress("10000003");
        plantItem.setFactoryCalendar("01");

        addressItem = Struct.create(Addresses.class);
        addressItem.setId(UUID.randomUUID().toString());
        addressItem.setAddress(plantItem.getAddress());

        companyCodeItem = Struct.create(CompanyCodes.class);
        companyCodeItem.setId(UUID.randomUUID().toString());
        companyCodeItem.setCompanyCode(plantItem.getCompanyCode());


        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);
    }

    @Test
    public void testBeforePlantsCreate(){
        plantHandler.beforePlantsCreate(createContextMock,plantItem);
    }


    @Test
    public void testBeforePlantsUpdate(){
        plantHandler.beforePlantsUpdate(updateEventContext,plantItem);
    }

    @Test
    public void testOnPlantsCreate(){
        Optional<Plants> opt = Optional.of(plantItem);
        Mockito.when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        Mockito.when(result.first(Plants.class)).thenReturn(opt);
        when(plantService.fetchPlantDetailsBasedOnNumber(plantItem.getId())).thenReturn(plantItem);

        plantHandler.onPlantsCreate(createContextMock,plantItem);
    }

    @Test
    public void testCreatePlant()  {
        Optional<Plants> emptyOpt = Optional.empty();
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(Plants.class)).thenReturn(emptyOpt);
        plantHandler.onPlantsCreate(createContextMock, plantItem);
    }



}
