package com.sap.ic.cmh.configuration.service;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ItemCategoryServiceImplTest {

    @InjectMocks
    ItemCategoryServiceImpl service;

    @Mock
    ItemCategoriesDao itemCategoriesDao;

    @Mock
    Result result;

    @Mock
    public Row row;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testGetComplaintCategoryItemCategoryMaps() {
        when(itemCategoriesDao.getItemCategoryDetails()).thenReturn(result);
        service.getItemCategories();
    }

    @Test
    public void testGetItemCategoryDetailsNullCheck() {
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        Optional<Row> opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        service.getItemCategoryDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }

    @Test
    public void testGetItemCategoryDetails() {
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ItemCategories itemCat = Struct.create(ItemCategories.class);
        itemCat.setId("ID");
        List<ItemCategories> itemCatList = new ArrayList<>();
        itemCatList.add(itemCat);
        when(result.listOf(ItemCategories.class)).thenReturn(itemCatList);
        service.getItemCategoryDetails("1e93d863-5b9d-4aae-9a96-375aab27270b");

    }
    @Test
    public void getActiveTest() {
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        row.put("id", "ID");
        Optional<Row> op = Optional.of(row);
        when(result.first()).thenReturn(op);
        ItemCategories itemCat = Struct.create(ItemCategories.class);
        itemCat.setId("ID");
        itemCat.setIsActive(true);
        List<ItemCategories> itemCatList = new ArrayList<>();
        itemCatList.add(itemCat);
        when(result.listOf(ItemCategories.class)).thenReturn(itemCatList);
        service.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
    @Test
    public void getActivenullTest() {
        when(itemCategoriesDao.getItemCategoryDetailsBasedOnId(anyString())).thenReturn(result);
        service.getActive("1e93d863-5b9d-4aae-9a96-375aab27270b");
    }
}

