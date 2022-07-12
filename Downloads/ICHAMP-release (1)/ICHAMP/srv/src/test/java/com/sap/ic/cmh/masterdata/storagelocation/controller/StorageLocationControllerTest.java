package com.sap.ic.cmh.masterdata.storagelocation.controller;

import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.service.StorageLocationService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class StorageLocationControllerTest {


    @InjectMocks
    @Autowired
    StorageLocationController controller;

    @Mock
    private StorageLocationService storageLocationService;

    private List<StorageLocationRequest> storageLocationRequest = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        StorageLocationRequest request = new StorageLocationRequest();
        request.setStorageLocation("StorageLocation");
        request.setRecordNo("12345");
        request.setPlantCode("F100");

        storageLocationRequest.add(request);
    }

    @Test
    public void testDeleteStorageLocationLists() throws Exception {
        controller.deleteStorageLocation(storageLocationRequest);
    }

    @Test
    public void testDeleteStorageLocationListNullTest() {
        controller.deleteStorageLocation(null);
    }


}
