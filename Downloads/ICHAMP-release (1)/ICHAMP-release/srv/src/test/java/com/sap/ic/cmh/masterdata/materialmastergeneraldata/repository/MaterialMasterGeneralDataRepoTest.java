package com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.*;
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

public class MaterialMasterGeneralDataRepoTest {
    @InjectMocks
    @Autowired
    MaterialMasterGeneralDataRepositoryImpl repository;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;

    private final List<String> companyCodeIds = new ArrayList<>();
    private final List<MaterialMasterGeneralDatas> companyCodesList = new ArrayList<>();
    private final List<Complaints> complaintsList = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        companyCodeIds.add("001");

        MaterialMasterGeneralDatas companyCodes = Struct.create(MaterialMasterGeneralDatas.class);
        companyCodes.setId("100");
        companyCodes.setBaseUnitOfMeasureCode("F001");
        companyCodes.setMaterialCode("F001");
        companyCodesList.add(companyCodes);

        Complaints complaints = Struct.create(Complaints.class);
        complaints.setCompanyCodeId("100");
        complaintsList.add(complaints);

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
    }

    @Test
    public void testGetMaterialMasterGeneralDataMap() {
        Map<String, String> companyCodeMap = new HashMap<>();
        companyCodeMap.put("100", "F001");
        when(result.listOf(MaterialMasterGeneralDatas.class)).thenReturn(companyCodesList);

        assertEquals(companyCodeMap, repository.getMaterialMasterGeneralDataMap(companyCodeIds));
    }

    @Test
    public void testGetActiveComplaintsInMaterialMasterGeneralData() {
        List<String> activeComplaintsList = new ArrayList<>();
        activeComplaintsList.add("100");
        when(result.listOf(Complaints.class)).thenReturn(complaintsList);
        repository.getActiveComplaintsInMaterialMasterGeneralData(companyCodeIds);
    }

    @Test
    public void testDeleteMaterialMasterGeneralDataList() {
        long deleteCount = 0;
        when(result.rowCount()).thenReturn(deleteCount);

       repository.deleteMaterialMasterGeneralDataList(companyCodeIds);
    }
    @Test
    public void testFetchMaterialMasterGeneralDataBasedOnCode() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        repository.fetchMaterialMasterGeneralDataBasedOnCode("dfd");
    }
    
    @Test
    public void testGetMaterialMasterGeneralDataBasedOnId() {
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        repository.getMaterialMasterGeneralDataBasedOnId("id");
    }
}