package com.sap.ic.cmh.configuration.model;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.configuration.handler.ServiceMaterialHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class MasterDataTest {
    @InjectMocks
    MasterData handler;
    @Mock
    private MaterialMasterGeneralDatas material;
    @Mock
    private BusinessPartners supplier;
    @Mock
    private Plants plants;
    @Mock
    private PurchaseOrganizations purchaseOrg;
    @Mock
    private BusinessPartners businessObjectService;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }
    @Test
    public void setMethodTest(){
        handler.setMaterial(material);
        handler.setPlants(plants);
        handler.setPurchaseOrg(purchaseOrg);
        handler.setSupplier(supplier);
        handler.setPersonResponsible(businessObjectService);
    }

    @Test
    public void getMethodTest(){
        handler.getMaterial();
        handler.getPlants();
        handler.getPurchaseOrg();
        handler.getSupplier();
        handler.getPersonResponsible();
    }

}
