package com.sap.ic.cmh.masterdata.plant.controller;


import com.sap.ic.cmh.masterdata.plant.model.PlantRequest;
import com.sap.ic.cmh.masterdata.plant.service.PlantServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;


public class PlantControllerTest {

    @InjectMocks
    @Autowired
    PlantController controller;

    @Mock
    private PlantServiceImpl plantService;

    List<PlantRequest> plantRequests = new ArrayList<>();

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);
        PlantRequest plantRequest = new PlantRequest();
        plantRequest.setPlant("Plant");
        plantRequest.setRecordNo("123344");

        plantRequests.add(plantRequest);
    }
    @Test
    public void testDeletePlantLists() {
        controller.deletePlants(plantRequests);
    }

    @Test
    public void testDeletePlantListsForNullValue()  {
        controller.deletePlants(null);
    }
}

