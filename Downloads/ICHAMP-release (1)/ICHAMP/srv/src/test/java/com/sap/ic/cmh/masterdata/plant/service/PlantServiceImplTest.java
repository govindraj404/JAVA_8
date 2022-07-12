package com.sap.ic.cmh.masterdata.plant.service;

import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchasingGroups;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.plant.model.PlantRequest;
import com.sap.ic.cmh.masterdata.plant.model.PlantResponse;
import com.sap.ic.cmh.masterdata.plant.repository.PlantRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class PlantServiceImplTest {

    @InjectMocks
    PlantServiceImpl plantServiceImpl;
    @Mock
    Result result;
    @Mock
    LocaleMessageHelper messageHelper;
    @Mock
    private PlantRepository plantRepository;
    private Plants  plantItem;
    private List<Plants> plantsList;
    private List<PlantRequest> plantDataRequestList;
    private List<String> plantList;

    @Before
    public void beforeClass() throws JsonProcessingException {
        MockitoAnnotations.openMocks(this);
        String json = "[{\"plant\":\"01\"},{\"plant\":\"02\"}]";
        plantDataRequestList =  new ObjectMapper().readValue(json, new TypeReference<List<PlantRequest>>() {
        });
        plantList = plantDataRequestList.stream().map(PlantRequest::getPlant).collect(Collectors.toList());

        plantItem = Struct.create(Plants.class);

        plantItem.setPlant("BP15");
        plantItem.setPlantName("SPL Chemical");
        plantItem.setPlantNameExtension("Plant");
        plantItem.setCompanyCode("BP02");
        plantItem.setCustomerNoOfPlant("100015");
        plantItem.setSupplierNoOfPlant("WERK4500");
        plantItem.setAddress("10000003");
        plantItem.setFactoryCalendar("01");
    }

    @Test
    public void testDeletePlantList_RecordsDeleted() {
        final List<PlantResponse> plantResponses = plantServiceImpl.deletePlantList(plantDataRequestList);
        Assert.assertEquals(2, plantResponses.size());
    }
    @Test
    public void testDeletePlantList() {
        Map<String, String> plantWithIdMap = new LinkedHashMap<>();
        plantWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        plantWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        when(plantRepository.getPlantMap(plantList)).thenReturn(plantWithIdMap);
        final List<PlantResponse> plantResponses = plantServiceImpl.deletePlantList(plantDataRequestList);
        Assert.assertEquals("01", plantResponses.get(0).getPlant());
    }
    @Test
    public void testDeletePlantLisElse() {
        Map<String, String> plantWithIdMap = new LinkedHashMap<>();
        plantWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
       // plantWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        when(plantRepository.getPlantMap(plantList)).thenReturn(plantWithIdMap);
        List<String> list=new ArrayList<>();
        list.add("01");
        when(plantRepository.getActiveComplaintsInPlant(any(List.class))).thenReturn(list);
         plantServiceImpl.deletePlantList(plantDataRequestList);

    }
    @Test
    public void testFetchSubItemTypesNull() {
        when(plantRepository.fetchPlantDetailsBasedOnNumber(plantItem.getPlant())).thenReturn(result);
        plantServiceImpl.fetchPlantDetailsBasedOnNumber(plantItem.getPlant());
    }


}
