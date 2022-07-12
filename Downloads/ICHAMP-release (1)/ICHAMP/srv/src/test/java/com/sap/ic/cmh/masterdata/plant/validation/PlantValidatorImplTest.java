package com.sap.ic.cmh.masterdata.plant.validation;

import cds.gen.masterdataservice.Plants;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;


public class PlantValidatorImplTest {

    @InjectMocks
    @Autowired
    PlantValidatorImpl plantValidatorImpl;

    @Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;

    private Plants plant;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);

        plant = Struct.create(Plants.class);
        plant.setPlant("BP15");
        plant.setPlantName("SPL Chemical");
        plant.setPlantNameExtension("Plant 11");
        plant.setCompanyCode("BP02");
        plant.setCustomerNoOfPlant("100015");
        plant.setSupplierNoOfPlant("WERK4500");
        plant.setAddress("10000003");
        plant.setFactoryCalendar("01");
    }

    @Test
    public void checkInputsSanitized_ValidationPass() {
       // Mockito.when(messageHelper.getMessage(Mockito.anyString())).thenReturn("");
        plantValidatorImpl.checkInputsSanitized(plant);
    }
}