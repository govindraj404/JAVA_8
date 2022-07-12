package com.sap.ic.cmh.masterdata.materialmastergeneraldata.service;

import cds.gen.masterdataservice.DefectGroups;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import com.sap.cds.Result;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class MaterialMasterGeneralDataServiceImplTest {
    @Mock
    private MaterialMasterGeneralDataRepository repository;
    @InjectMocks
    private MaterialMasterGeneralDataServiceImpl services;
    @Mock
    LocaleMessageHelper messageHelper;

    @Mock
    protected PersistenceService db;
    @Mock
    Result result;
    List<MaterialMasterGeneralDataRequest> dataList = new ArrayList<>();
    private  Map<String, String> dataMap;
    List<MaterialMasterGeneralDatas> generalList = new ArrayList<>();
    private MaterialMasterGeneralDatas generaldatas;
     private  String id = null;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        MaterialMasterGeneralDataRequest request=new MaterialMasterGeneralDataRequest() ;
        request.setMaterialCode("101");
        request.setRecordNo("21");
        id = UUID.randomUUID().toString();
        dataList.add(request);
        dataMap = new HashMap<>();
        dataMap.put("101", "1");
    }


    @Test
    public void deleteMaterialDataTest() {
        dataMap = new HashMap<>();
        dataMap.put(id, "4052c63c-6351-4ea5-8192-4af6686bb526");
        List<String> listofData=new ArrayList<>();
        listofData.add("101");
        when(repository.getMaterialMasterGeneralDataMap(listofData)).thenReturn(dataMap);

        List<MaterialMasterGeneralDataResponse> materialMasterGeneralDataResponses = services.deleteMaterialMasterGeneralDataList(dataList);
        Assert.assertEquals("101", materialMasterGeneralDataResponses.get(0).getMaterialCode());
        Assert.assertNotNull(materialMasterGeneralDataResponses.toString());
    }

    @Test
    public void deleteMaterialDataElseTest() {
        dataMap = new HashMap<>();
        dataMap.put(id, "4052c63c-6351-4ea5-8192-4af6686bb526");
        List<String> listofData=new ArrayList<>();
        //listofData.add("101");
        List<String> listofData1=new ArrayList<>();
        when(repository.getMaterialMasterGeneralDataMap(listofData)).thenReturn(dataMap);
        when(repository.getActiveComplaintsInMaterialMasterGeneralData(listofData)).thenReturn(listofData1);
        List<MaterialMasterGeneralDataResponse> materialMasterGeneralDataResponses = services.deleteMaterialMasterGeneralDataList(dataList);
        Assert.assertEquals("101", materialMasterGeneralDataResponses.get(0).getMaterialCode());
        Assert.assertNotNull(materialMasterGeneralDataResponses.toString());
    }
    @Test
    public void testDeleteMaterialDataElse() {
        Map<String, String> materialDataWithIdMap = new LinkedHashMap<>();
        materialDataWithIdMap.put("01", "4052c63c-6351-4ea5-8192-4af6686bb526");
        // plantWithIdMap.put("21", "4052c63c-6351-4ea5-8192-4af6686bb527");
        List<String> listofData=new ArrayList<>();
        listofData.add("101");
        when(repository.getMaterialMasterGeneralDataMap(listofData)).thenReturn(materialDataWithIdMap);
        List<String> list=new ArrayList<>();
        list.add("01");
        when(repository.getActiveComplaintsInMaterialMasterGeneralData(any(List.class))).thenReturn(list);
        services.deleteMaterialMasterGeneralDataList(dataList);


    }


    @Test
    public void testFetchMaterialMasterGeneralDataBasedOnCodeNull() {
        when(repository.fetchMaterialMasterGeneralDataBasedOnCode("hhh")).thenReturn(result);
        services.fetchMaterialMasterGeneralDataBasedOnCode("hhh");


    }
}
