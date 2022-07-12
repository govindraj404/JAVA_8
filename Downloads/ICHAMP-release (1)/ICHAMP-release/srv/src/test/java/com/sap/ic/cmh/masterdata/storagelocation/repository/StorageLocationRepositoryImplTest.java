package com.sap.ic.cmh.masterdata.storagelocation.repository;

import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations_;
import cds.gen.masterdataservice.StorageLocations;
import cds.gen.masterdataservice.StorageLocations_;
import com.sap.cds.Result;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class StorageLocationRepositoryImplTest {

    @InjectMocks
    @Autowired
    StorageLocationRepositoryImpl StorageLocationRepositoryImpl;

    @Mock
    private PersistenceService mockDb;

    @Mock
    Result result;

    private List<StorageLocations> storageLocationList = new ArrayList<>();
    private List<String> storageLocationStringList = new ArrayList<>();
    private List<String> plantCodeList  = new ArrayList<>();
    private StorageLocations storageLocations;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        storageLocations = StorageLocations.create();
        storageLocations.setId("200001");
        storageLocations.setPlantCode("F001");
        storageLocations.setStorageLocation("Australia");
        storageLocationList.add(storageLocations);

        storageLocationStringList.add("100001");
        plantCodeList.add("100002");

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }

    @Test
    public void testGetStorageLocationMap(){
        Map<String, String> storageLocationMap = new HashMap<>();
        storageLocationMap.put("200001", "Australia");
        when(result.listOf(StorageLocations.class)).thenReturn(storageLocationList);
        assertEquals(storageLocationMap, StorageLocationRepositoryImpl.getStorageLocationMap(storageLocationStringList,plantCodeList));
    }

    @Test
    public void testDeleteStorageLocationList(){
        CqnInsert cqnInsert = Insert.into(StorageLocations_.class).entry(storageLocations);
        mockDb.run(cqnInsert);
        StorageLocationRepositoryImpl.deleteStorageLocationList(storageLocationStringList);


    }
    @Test
    public void testFetchStorageLocations() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        StorageLocationRepositoryImpl.fetchStorageLocations("ff","e");

    }
}
