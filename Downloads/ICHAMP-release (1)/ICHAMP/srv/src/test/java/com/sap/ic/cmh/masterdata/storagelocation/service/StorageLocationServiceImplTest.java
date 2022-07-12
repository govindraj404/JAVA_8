package com.sap.ic.cmh.masterdata.storagelocation.service;

import cds.gen.masterdataservice.StorageLocations;
import cds.gen.masterdataservice.SubItemTypes;
import cds.gen.masterdataservice.UnitOfMeasures;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.plant.repository.PlantRepository;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationResponse;
import com.sap.ic.cmh.masterdata.storagelocation.repository.StorageLocationRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class StorageLocationServiceImplTest {
    @InjectMocks
    StorageLocationServiceImpl storageLocationServiceImpl;

    @Mock
    LocaleMessageHelper messageHelper;
    @Mock
    Result result;
    @Mock
    private StorageLocationRepository storageLocationRepository;

    @Mock
    private PlantRepository plantRepository;
    private Row row;
    private Optional<Row> opt;
    public List<StorageLocationRequest> storageLocationRequestList;

    public StorageLocations storageLocation;

    public String json = "[{\"plantCode\":\"10000001\",\"storageLocation\":\"10000001\"},{\"plantCode\":\"10000002\",\"storageLocation\":\"10000002\"}]";

    private List<StorageLocations> storageLocationLists = new ArrayList<>();

    public List<String> storageLocationList = new ArrayList<>();

    public List<String> plantCodeList = new ArrayList<>();

    public Map<String,String> storageLocationMap = new HashMap<>();

    Map<String, String> plantWithIdMap = new HashMap<>();

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);
        storageLocation = Struct.create(StorageLocations.class);
        storageLocation.setStorageLocation("10000001");
        storageLocation.setId("10000001");
        storageLocation.setPlantCode("10000001");

        storageLocationLists.add(storageLocation);
        storageLocationRequestList = new ObjectMapper().readValue(json, new TypeReference<List<StorageLocationRequest>>() {
        });

        plantCodeList.add("10000001");
        plantCodeList.add("10000002");

        storageLocationList.add("10000001");
        storageLocationList.add("10000002");
        storageLocationMap.put("10000001","10000001");
        plantWithIdMap.put("10000001","10000001");

    }

    @Test
    public void testDeleteStorageLocationList() {
        Mockito.when(storageLocationRepository.getStorageLocationMap(storageLocationList,plantCodeList)).thenReturn(storageLocationMap);
        Mockito.when(plantRepository.getPlantMap(plantCodeList)).thenReturn(plantWithIdMap);
        List<StorageLocationResponse> storageLocationResponse = storageLocationServiceImpl.deleteStorageLocationList(storageLocationRequestList);
        assertEquals("10000002", storageLocationResponse.get(0).getStorageLocation());
        assertEquals("10000002", storageLocationResponse.get(0).getPlantCode());
    }
    @Test
    public void testDeleteStorageLocationListElse() {
        Mockito.when(storageLocationRepository.getStorageLocationMap(storageLocationList,plantCodeList)).thenReturn(storageLocationMap);
        Mockito.when(plantRepository.getPlantMap(plantCodeList)).thenReturn(plantWithIdMap);
        List<String> list=new ArrayList<>();
        list.add("10000001");
        when(plantRepository.getActiveComplaintsInPlant(any(List.class))).thenReturn(list);
        when(plantRepository.getPlantId(any(String.class))).thenReturn(list.get(0));
         storageLocationServiceImpl.deleteStorageLocationList(storageLocationRequestList);

    }

    @Test
    public void testFetchSubItemTypesNull() {
        when(storageLocationRepository.fetchStorageLocations(storageLocation.getStorageLocation(),
        storageLocation.getPlantCode())).thenReturn(result);
        storageLocationServiceImpl.fetchStorageLocations(storageLocation.getStorageLocation(),
                storageLocation.getPlantCode());
    }

}
