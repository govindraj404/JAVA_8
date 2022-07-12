package com.sap.ic.cmh.masterdata.plant.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
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

public class PlantRepositoryImplTest {

    @InjectMocks
    @Autowired
    PlantRepositoryImpl plantRepositoryImpl;

    @Mock
    private PersistenceService mockDb;

    @Mock
    Result result;

    private List<Plants> plantList = new ArrayList<>();

    private  List<String> plantIds = new ArrayList<>();

    private Plants plants;

    private List<Complaints> complaintsList = new ArrayList<>();
    private Complaints complaints;
    private String plantId;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        plantId = "100001";
        plants = Plants.create();
        plants.setId("100001");
        plants.setPlant("100002");

        plantList.add(plants);
        plantIds.add("100001");

        complaints = Complaints.create();
        complaints.setPlantId("100");
        complaints.setComplaintStatusCode("CLSD");
        complaintsList.add(complaints);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);

    }

    @Test
    public void testPlantMap() {
        Map<String, String> plantMap = new HashMap<>();
        plantMap.put("100001", "100002");
        when(result.listOf(Plants.class)).thenReturn(plantList);
        assertEquals(plantMap, plantRepositoryImpl.getPlantMap(plantIds));

    }

    @Test
    public void testDeletePlant() {
        CqnInsert cqnInsert = Insert.into(Plants_.class).entry(plants);
        mockDb.run(cqnInsert);
        plantRepositoryImpl.deletePlantList(plantIds);
    }

    @Test
    public void testGetPlantId(){
        String plantId = "100001";
        when(result.listOf(Plants.class)).thenReturn(plantList);
        assertEquals(plantId , plantRepositoryImpl.getPlantId(plantId));
     }
    @Test
    public void testGetActiveComplaintsInPlant() {

        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Complaints.class)).thenReturn(complaintsList);
        assertEquals(activeComplaintsList,plantRepositoryImpl.getActiveComplaintsInPlant(plantIds));

    }
    @Test
    public void testFetchPlantDetailsBasedOnNumber() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        plantRepositoryImpl.fetchPlantDetailsBasedOnNumber("jj"); }

}
