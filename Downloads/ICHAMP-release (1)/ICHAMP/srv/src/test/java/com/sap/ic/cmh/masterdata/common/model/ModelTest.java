package com.sap.ic.cmh.masterdata.common.model;

import com.sap.ic.cmh.masterdata.address.model.AddressResponse;
import com.sap.ic.cmh.masterdata.companycode.model.CompanyCodeResponse;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.model.MaterialMasterGeneralDataResponse;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataRequest;
import com.sap.ic.cmh.masterdata.materialmasterplantdata.model.MaterialMasterPlantDataResponse;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ModelTest {
    @Mock
    AddressResponse addressResponse;
    @Mock
    CompanyCodeResponse companyCodeResponse;
    @Mock
    MaterialMasterGeneralDataResponse materialMasterGeneralDataResponse;
    @Mock
    MaterialMasterPlantDataRequest materialMasterPlantDataRequest;
    @Mock
    MaterialMasterPlantDataResponse materialMasterPlantDataResponse;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testSetMethod() {
        addressResponse.setStatus("200");
        addressResponse.setAddress("test");
        addressResponse.toString();
        companyCodeResponse.setCompanyCode("F01");
        companyCodeResponse.setStatus("200");
        companyCodeResponse.setRecordNo("300");
        companyCodeResponse.setMessage("ds");
        companyCodeResponse.toString();
        materialMasterGeneralDataResponse.setMaterialCode("f01");
        materialMasterGeneralDataResponse.toString();
        materialMasterPlantDataRequest.setPlant("200");
        materialMasterPlantDataRequest.setMaterialCode("Mat");
        materialMasterPlantDataRequest.toString();
        materialMasterPlantDataResponse.setMaterialCode("200");
        materialMasterPlantDataResponse.setMessage("sucess");
        materialMasterPlantDataResponse.toString();
    }
    @Test
    public void testGetMethod() {
        addressResponse.getAddress();
        addressResponse.getMessage();
        addressResponse.getStatus();
        companyCodeResponse.getCompanyCode();
        companyCodeResponse.getStatus();
        companyCodeResponse.getRecordNo();
        companyCodeResponse.getMessage();
        companyCodeResponse.toString();
        materialMasterGeneralDataResponse.getMaterialCode();
        materialMasterPlantDataRequest.getPlant();
        materialMasterPlantDataRequest.getMaterialCode();
        materialMasterPlantDataResponse.getMaterialCode();
        materialMasterPlantDataResponse.getMessage();

    }

    @Test
    public void testEqualAndHashCode() {
        CompanyCodeResponse sw = new CompanyCodeResponse();
        sw.setMessage("df");
        sw.setStatus("ee");
        sw.setCompanyCode("vf");
        sw.setRecordNo("232");
        sw.toString();
        CompanyCodeResponse sx = new CompanyCodeResponse();
        sx.setMessage("df");
        sx.setStatus("ee");
        sx.setCompanyCode("vf");
        sx.setRecordNo("232");
       // assertEquals(sw, sx);
      //  assertTrue( sw.hashCode()==sx.hashCode());
        StorageLocationRequest s1 = new StorageLocationRequest();
        s1.setPlantCode("001");
        s1.setStorageLocation("India");
        s1.setRecordNo("1");
        s1.toString();
        StorageLocationRequest s2 = new StorageLocationRequest( );
        s2.setPlantCode("001");
        s2.setStorageLocation("India");
        s2.setRecordNo("1");
        assertEquals(s1, s2);
        assertTrue( s1.hashCode()==s2.hashCode());
    }

}
