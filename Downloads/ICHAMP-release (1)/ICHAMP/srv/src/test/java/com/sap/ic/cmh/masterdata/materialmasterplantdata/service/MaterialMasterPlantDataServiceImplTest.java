package com.sap.ic.cmh.masterdata.materialmasterplantdata.service;


import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterPlantDatas;
import cds.gen.masterdataservice.PurchasingGroups;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.repository.MaterialMasterPlantDataRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;


public class MaterialMasterPlantDataServiceImplTest {

    @InjectMocks
    private MaterialMasterPlantDataServiceImpl services;

    @Mock
    private MaterialMasterPlantDataRepository repository;

    @Mock
    private MaterialMasterGeneralDataRepository repository1;
    @Mock
    Result result;
    @Mock
    LocaleMessageHelper messageHelper;

     private MaterialMasterPlantDatas materialMasterPlantDatas;
    List<MaterialMasterPlantDatas> list12 = new ArrayList<>();
    private List<MaterialMasterPlantDataRequest> materialMasterPlantDataRequestList = new ArrayList<>();
    private List<String> MaterialMasterPlantDataList = new ArrayList<>();
    private List<String> plantDataList = new ArrayList<>();
    List<MaterialMasterGeneralDatas> list = new ArrayList<>();
    List<MaterialMasterGeneralDatas> list1 = new ArrayList<>();
    List<String> materialDBIDs = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        MaterialMasterPlantDataRequest request = new MaterialMasterPlantDataRequest();
        request.setMaterialCode("01");
        request.setRecordNo("33");
        request.setPlant("F002");
        materialMasterPlantDataRequestList.add(request);
        MaterialMasterPlantDataList = materialMasterPlantDataRequestList.stream().map(MaterialMasterPlantDataRequest::getMaterialCode).collect(Collectors.toList());
        plantDataList = materialMasterPlantDataRequestList.stream().map(MaterialMasterPlantDataRequest::getPlant).collect(Collectors.toList());

        MaterialMasterGeneralDatas datas = Struct.create(MaterialMasterGeneralDatas.class);
        datas.setBaseUnitOfMeasure(null);
        datas.setId("01");
        datas.setMaterialCode("01");
        list.add(datas);
        materialDBIDs.add ("01");
    }

    @Test
    public void testDeleteMaterialMasterPlantData_RecordsDeleted() {
        final List<MaterialMasterPlantDataResponse> materialMasterPlantDataResponse = services.deleteMaterialMasterPlantDataList(materialMasterPlantDataRequestList);
        Assert.assertEquals(1, materialMasterPlantDataResponse.size());
        Assert.assertNotNull(materialMasterPlantDataResponse.toString());
    }
    @Test
    public void testDeleteMaterialMasterPlantData() {
        Map<String, String>  materialMasterWithIdMap = new LinkedHashMap<>();
        materialMasterWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        materialMasterWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        Mockito.when(repository.getMaterialMasterGeneralDataList(anyList())).thenReturn(list);
        Mockito.when(repository1.getActiveComplaintsInMaterialMasterGeneralData(anyList())).thenReturn(materialDBIDs);
        Mockito.when(repository.getMaterialMasterPlantDataMap(MaterialMasterPlantDataList , plantDataList)).thenReturn(materialMasterWithIdMap);
        final List<MaterialMasterPlantDataResponse> materialMasterPlantDataResponse = services.deleteMaterialMasterPlantDataList(materialMasterPlantDataRequestList);
        Assert.assertEquals("01", materialMasterPlantDataResponse.get(0).getMaterialCode());
    }

    @Test
    public void testDeleteRecordMaterialMasterPlantData() {
        MaterialMasterGeneralDatas datas = Struct.create(MaterialMasterGeneralDatas.class);
        datas.setBaseUnitOfMeasure(null);
        datas.setId("101");
        datas.setMaterialCode("01");
        list1.add(datas);
        Map<String, String>  materialMasterWithIdMap = new LinkedHashMap<>();
        materialMasterWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        materialMasterWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        Mockito.when(repository.getMaterialMasterGeneralDataList(anyList())).thenReturn(list1);
        Mockito.when(repository1.getActiveComplaintsInMaterialMasterGeneralData(anyList())).thenReturn(materialDBIDs);
        Mockito.when(repository.getMaterialMasterPlantDataMap(MaterialMasterPlantDataList , plantDataList)).thenReturn(materialMasterWithIdMap);
        services.deleteMaterialMasterPlantDataList(materialMasterPlantDataRequestList);
    }
    @Test
    public void testFetchSubItemTypesNull() {
        when(repository.fetchMaterialMasterPlantDatas("f","htht")).thenReturn(result);
        services.fetchMaterialMasterPlantDatas("f","htht");
    }

}
