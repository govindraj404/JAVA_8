package com.sap.ic.cmh.masterdata.storagelocation.handler;

import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.StorageLocations;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.plant.service.PlantService;
import com.sap.ic.cmh.masterdata.storagelocation.service.StorageLocationService;
import com.sap.ic.cmh.masterdata.storagelocation.validation.StorageLocationValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class StorageLocationHandlerTest {

    @InjectMocks
    @Autowired
    StorageLocationHandler storageLocationHandler;

    @Autowired
    StorageLocationService storageLocationService;


    @Mock
    private PersistenceService dbMock;

    @Mock
    PlantService plantService;

    @Mock
    private Messages messages;

    @Mock
    private StorageLocationValidator storageLocationValidator;

    @Mock
    CdsCreateEventContext createContextMock;
    @Mock
    CdsUpdateEventContext context;
    @Mock
    CqnInsert cqnInsert;

    @Mock
    Result result;

    @Mock
    StorageLocationValidator validator;
    private StorageLocations storageLocationItem;

    private Plants plantItem;
    private StorageLocations storageLocations;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        storageLocationItem = Struct.create(StorageLocations.class);

        storageLocationItem.setStorageLocation("0002");
        storageLocationItem.setStorageLocationName("Production");
        storageLocationItem.setPlantCode("BP11");

        storageLocations = Struct.create(StorageLocations.class);

        storageLocations.setStorageLocation("0002");
        storageLocations.setStorageLocationName("Production");
        storageLocations.setPlantCode("BP11");
        plantItem = Struct.create(Plants.class);
        plantItem.setId(UUID.randomUUID().toString());
        plantItem.setPlant(storageLocationItem.getPlantCode());

        when(createContextMock.getCqn()).thenReturn(cqnInsert);
        List<Map<String, Object>> entries = new ArrayList<>();
        when(cqnInsert.entries()).thenReturn(entries);


    }


    @Test
    public void testCreateStorageLocations()  {
        Optional<StorageLocations> emptyOpt = Optional.empty();
        when(dbMock.run(any(CqnSelect.class))).thenReturn(result);
        doNothing().when(validator).checkInputsSanitized(storageLocationItem);
        when(result.first(StorageLocations.class)).thenReturn(emptyOpt);
        Mockito.when(plantService.fetchPlant(any(), any(), any(), any())).thenReturn(plantItem);
        storageLocationHandler.beforeCreateStorageLocation(createContextMock, storageLocationItem);
    }



    @Test
    public void testUpdateStorageLocationsPlantIsNull() {
        Plants emptyPlantItem = Struct.create(Plants.class);
        Optional<StorageLocations> opt = Optional.of(storageLocationItem);
        Mockito.when(dbMock.run(any(CqnSelect.class))).thenReturn(result);

        Mockito.when(result.first(StorageLocations.class)).thenReturn(opt);
        doNothing().when(validator).checkInputsSanitized(storageLocationItem);
        Mockito.when(plantService.fetchPlant(any(), any(), any(), any())).thenReturn(emptyPlantItem);

        storageLocationHandler.beforeStorageLocationUpdate(context, storageLocationItem);
    }
}
