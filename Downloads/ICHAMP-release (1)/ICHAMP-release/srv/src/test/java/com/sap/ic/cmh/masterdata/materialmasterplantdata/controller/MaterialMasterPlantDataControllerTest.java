package com.sap.ic.cmh.masterdata.materialmasterplantdata.controller;

import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.service.MaterialMasterPlantDataService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;


public class MaterialMasterPlantDataControllerTest {

    @Mock
    private MaterialMasterPlantDataService service;
    static List<MaterialMasterPlantDataResponse> responseList;
    List<MaterialMasterPlantDataRequest> addressRequests;
    @InjectMocks
    private MaterialMasterPlantDataController controller;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        MaterialMasterPlantDataResponse response=new MaterialMasterPlantDataResponse();

        response.setMessage("test");
        response.setRecordNo("2");
        response.setStatus("success");
    }
    @Test
    public void  deleteMasterDataList() throws Exception{
        MaterialMasterPlantDataRequest request1 = new MaterialMasterPlantDataRequest();
        request1.setMaterialCode("F01");
        MaterialMasterPlantDataRequest request2 = new MaterialMasterPlantDataRequest();
        request2.setMaterialCode("F01");

        addressRequests = new ArrayList<>();
        addressRequests.add(request1);
        addressRequests.add(request2);

        when(service.deleteMaterialMasterPlantDataList(addressRequests)).thenReturn(responseList);


    }
    @Test
    public void deleteMasterDataListTest()
    {
        controller.deleteMaterialMasterPlantDataLists(addressRequests);
    }
    @Test
    public void deleteMasterDataListNullTest()
    {
        controller.deleteMaterialMasterPlantDataLists(null);
    }
}
