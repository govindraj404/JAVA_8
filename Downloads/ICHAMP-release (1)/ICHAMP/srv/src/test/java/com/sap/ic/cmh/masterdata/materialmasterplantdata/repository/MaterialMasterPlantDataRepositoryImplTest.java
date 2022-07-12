package com.sap.ic.cmh.masterdata.materialmasterplantdata.repository;

import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.Plants;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
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

public class MaterialMasterPlantDataRepositoryImplTest {
    @InjectMocks
    @Autowired
    MaterialMasterPlantDataRepositoryImpl repository;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;

    private final List<String> companyCodeIds = new ArrayList<>();
    private final List<MaterialMasterPlantDatas> companyCodesList = new ArrayList<>();
    private final List<Plants> complaintsList = new ArrayList<>();
    private MaterialMasterPlantDatas MaterialMasterPlantDatas;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        companyCodeIds.add("001");

        MaterialMasterPlantDatas companyCodes = Struct.create(MaterialMasterPlantDatas.class);
        companyCodes.setId("100");
        companyCodes.setMaterialCode("F001");
        companyCodesList.add(companyCodes);

        Plants complaints = Struct.create(Plants.class);
        complaints.setId("100");
        complaintsList.add(complaints);

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }

    @Test
    public void testGetMaterialMasterPlantDataMap() {
        Map<String, String> companyCodeMap = new HashMap<>();
        companyCodeMap.put("100", "F001");
        when(result.listOf(MaterialMasterPlantDatas.class)).thenReturn(companyCodesList);
         repository.getMaterialMasterPlantDataMap(companyCodeIds,companyCodeIds);
    }

    @Test
    public void testGetMaterialMasterGeneralDataList() {
        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Plants.class)).thenReturn(complaintsList);
        repository.getMaterialMasterGeneralDataList(companyCodeIds);
    }

    @Test
    public void testDeleteMaterialMasterGeneralDataList() {
        long deleteCount = 0;
        when(result.rowCount()).thenReturn(deleteCount);

        repository.deleteMaterialMasterPlantDataList(companyCodeIds);
    }
    @Test
    public void testFetchMaterialMasterPlantDatas() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        repository.fetchMaterialMasterPlantDatas("ewe","wrw");
    }

}

