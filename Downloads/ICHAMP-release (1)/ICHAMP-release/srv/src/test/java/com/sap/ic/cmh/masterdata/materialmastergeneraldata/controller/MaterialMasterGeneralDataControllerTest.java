package com.sap.ic.cmh.masterdata.materialmastergeneraldata.controller;

import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataRequest;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.service.MaterialMasterGeneralDataService;
import org.junit.Before;

import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;


public class MaterialMasterGeneralDataControllerTest{

    @Mock
    private MaterialMasterGeneralDataService service;
    static List<MaterialMasterGeneralDataResponse> responseList;
    List<MaterialMasterGeneralDataRequest> addressRequests;
    @InjectMocks
    private MaterialMasterGeneralDataController controller;

    @Before
    public void beforeClass()  {
        MockitoAnnotations.openMocks(this);
        MaterialMasterGeneralDataResponse response=new MaterialMasterGeneralDataResponse();
        response.setMessage("test");
        response.setRecordNo("2");
        response.setStatus("success");
    }
    @Test
    public void  deleteMasterDataList() throws Exception{
        MaterialMasterGeneralDataRequest request1 = new MaterialMasterGeneralDataRequest();
        request1.setMaterialCode("F01");
        MaterialMasterGeneralDataRequest request2 = new MaterialMasterGeneralDataRequest();
        request2.setMaterialCode("F01");

        addressRequests = new ArrayList<>();
        addressRequests.add(request1);
        addressRequests.add(request2);

        controller.deleteMaterialMasterGeneralDataLists(addressRequests);

    }

    @Test
    public void deleteMasterDataListTest()
    {
        controller.deleteMaterialMasterGeneralDataLists(addressRequests);
    }
    @Test
    public void deleteMasterDataListNullTest()
    {
        controller.deleteMaterialMasterGeneralDataLists(null);
    }
}
