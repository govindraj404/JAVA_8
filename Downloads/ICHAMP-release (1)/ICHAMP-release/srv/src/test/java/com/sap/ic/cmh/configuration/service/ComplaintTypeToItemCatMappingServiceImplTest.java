package com.sap.ic.cmh.configuration.service;


import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToItemCatMappingDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;

public class ComplaintTypeToItemCatMappingServiceImplTest {

    @InjectMocks
    ComplaintTypeToItemCatMappingServiceImpl complaintTypeToItemCatMappingServiceImpl;

    @Mock
    ComplaintTypeToItemCatMappingDao complaintTypeToItemCatMappingDao;
    @Mock
    public Row row;
    @Mock
    Result result;
    @Mock
    ComplaintTypeConfigurationDao complaintTypeConfigurationDao;
    @Mock
    ItemCategoriesDao itemCategoriesDao;
    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void getComplaintTypeToItemCatMappingTest(){
        complaintTypeToItemCatMappingServiceImpl.getComplaintTypeToItemCatMapping();
    }

    @Test
    public void getComplaintTypeToItemCatMappingsDetailsNullTest(){
        when(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatBasedOnId(
                any(String.class))).thenReturn(result);
        Optional<Row> opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        complaintTypeToItemCatMappingServiceImpl.getComplaintTypeToItemCatMappingsDetails("ID");
    }
    @Test
    public void getComplaintTypeToItemCatMappingsDetailsTest(){
        when(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatBasedOnId(any(String.class))).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ComplaintTypeToItemCategoryMappings comp = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        comp.setId("ID");
        List<ComplaintTypeToItemCategoryMappings> compList = new ArrayList<>();
        compList.add(comp);
        when(result.listOf(ComplaintTypeToItemCategoryMappings.class)).thenReturn(compList);
        complaintTypeToItemCatMappingServiceImpl.getComplaintTypeToItemCatMappingsDetails("ID");
    }

    @Test
    public void getIsComplaintTypeActiveTrueTest() {
        List<ComplaintTypeConfigurations> ComplaintTypeConfigurationsList = new ArrayList();
        ComplaintTypeConfigurations complaintType = Struct.create(ComplaintTypeConfigurations.class);
        complaintType.setId("id");
        complaintType.setCode("CODE100");
        complaintType.setIsActive(true);
        ComplaintTypeConfigurationsList.add(complaintType);
        Optional<Row> rowOptional = Optional.of(row);
        when(complaintTypeConfigurationDao.getComplaintTypeDetailsBasedOnId(anyString())).thenReturn(result);
        when(result.first()).thenReturn(rowOptional);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ComplaintTypeConfigurationsList);
        complaintTypeToItemCatMappingServiceImpl.getIsComplaintTypeActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
    @Test
    public void getIsItemCategoryActiveTrueTest() {
        List<ItemCategories> itemCategoriesList = new ArrayList();
        ItemCategories itemCategories = Struct.create(ItemCategories.class);
        itemCategories.setId("id");
        itemCategories.setCode("CODE100");
        itemCategories.setIsActive(true);
        itemCategoriesList.add(itemCategories);
        Optional<Row> rowOptional = Optional.of(row);
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        when(result.first()).thenReturn(rowOptional);
        when(result.listOf(ItemCategories.class)).thenReturn(itemCategoriesList);
        complaintTypeToItemCatMappingServiceImpl.getIsItemCategoryActive("1e93d863-5b9b-4aae-9a96-375aab27270c");
    }
    @Test
    public void getIsComplaintTypeActiveRowEmptyTest() {
        List<ComplaintTypeConfigurations> ComplaintTypeConfigurationsList = new ArrayList();
        ComplaintTypeConfigurations complaintType = Struct.create(ComplaintTypeConfigurations.class);
        complaintType.setId("id");
        complaintType.setCode("CODE100");
        complaintType.setIsActive(false);
        ComplaintTypeConfigurationsList.add(complaintType);
        Optional<Row> rowOptional = Optional.empty();
        when(complaintTypeConfigurationDao.getComplaintTypeDetailsBasedOnId(anyString())).thenReturn(result);
        when(result.first()).thenReturn(rowOptional);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ComplaintTypeConfigurationsList);
        complaintTypeToItemCatMappingServiceImpl.getIsComplaintTypeActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
    @Test
    public void getIsItemCategoryActiveRowEmptyTest() {
        List<ItemCategories> itemCategoriesList = new ArrayList();
        ItemCategories itemCategories = Struct.create(ItemCategories.class);
        itemCategories.setId("id");
        itemCategories.setCode("CODE100");
        itemCategories.setIsActive(false);
        itemCategoriesList.add(itemCategories);
        Optional<Row> rowOptional = Optional.empty();
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        when(result.first()).thenReturn(rowOptional);
        when(result.listOf(ItemCategories.class)).thenReturn(itemCategoriesList);
        complaintTypeToItemCatMappingServiceImpl.getIsItemCategoryActive("1e93d863-5b9b-4aae-9a96-375aab27270c");
    }
}
