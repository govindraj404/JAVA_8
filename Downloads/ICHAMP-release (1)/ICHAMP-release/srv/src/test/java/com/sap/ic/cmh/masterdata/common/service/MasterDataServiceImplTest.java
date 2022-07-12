package com.sap.ic.cmh.masterdata.common.service;

import cds.gen.masterdataservice.*;
import cds.gen.sap.common.Currencies;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.masterdata.unitofmeasure.persistency.UnitOfMeasureRepository;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MasterDataServiceImplTest {

    @InjectMocks
    @Autowired
    MasterDataServiceImpl masterDataServiceImpl;
    @Mock
    MasterDataDao masterDataDao;
    @Mock
    UnitOfMeasureRepository unitOfMeasureRepo;
    @Mock
    Result result;
    @Before
    public void beforeClass()  {
        MockitoAnnotations.openMocks(this);
    }


    @Test
    public void testGetMaterial() {
        MaterialMasterGeneralDatas datas = Struct.create(MaterialMasterGeneralDatas.class);
        datas.setMaterialCode("12344");
        datas.setId("234445");
        when(masterDataDao.getMaterial(any(String.class))).thenReturn(datas);
        MaterialMasterGeneralDatas material = masterDataServiceImpl.getMaterial(datas.getId());
        Assert.assertEquals("234445" , material.getId());
    }


    @Test
    public void testGetSupplier() {
        BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
        businessPartners.setAddress("Australia");
        businessPartners.setId("234445");
        when(masterDataDao.getSupplier(any(String.class))).thenReturn(businessPartners);
        BusinessPartners businessPartnerResponse = masterDataServiceImpl.getSupplier(businessPartners.getId());
        Assert.assertEquals("234445" , businessPartnerResponse.getId());

    }
    @Test
    public void testGetPlant() {
        Plants plants = Struct.create(Plants.class);
        plants.setAddress("Australia");
        plants.setId("234445");
        when(masterDataDao.getPlant(any(String.class))).thenReturn(plants);
        Plants plantsResponse = masterDataServiceImpl.getPlant(plants.getId());
        Assert.assertEquals("234445" , plantsResponse.getId());

    }


    @Test
    public void testGetCompanyCode() {
        CompanyCodes companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setAddress("Australia");
        companyCodes.setId("234445");
        when(masterDataDao.getCompanyCode(any(String.class))).thenReturn(companyCodes);
        CompanyCodes companyCodesResponse = masterDataServiceImpl.getCompanyCode(companyCodes.getId());
        Assert.assertEquals("234445" , companyCodesResponse.getId());

    }

    @Test
    public void testGetPurchasingOrganization() {
        PurchaseOrganizations purchaseOrganizations = Struct.create(PurchaseOrganizations.class);
        purchaseOrganizations.setPurchaseOrganization("12345");
        purchaseOrganizations.setId("234445");
        when(masterDataDao.getPurchasingOrganization(any(String.class))).thenReturn(purchaseOrganizations);
        PurchaseOrganizations purchaseOrganizationsResponse = masterDataServiceImpl.getPurchasingOrganization(purchaseOrganizations.getId());
        Assert.assertEquals("234445" , purchaseOrganizationsResponse.getId());

    }

    @Test
    public void testGetCurrency() {
        Currencies currencies = Struct.create(Currencies.class);
        currencies.setCode("INR");
        currencies.setDescr("India currency");
        when(masterDataDao.getCurrency(any(String.class))).thenReturn(currencies);
        Currencies CurrenciesResponse = masterDataServiceImpl.getCurrency(currencies.getCode());
        Assert.assertEquals("INR" , CurrenciesResponse.getCode());

    }
    @Test
    public void testValidateCurrencyBasedOnCompanyCode() {
        CompanyCodes companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setAddress("Australia");
        companyCodes.setId("234445");
        when(masterDataDao.validateCurrencyBasedOnCompanyCode(any(String.class))).thenReturn(companyCodes);
        CompanyCodes companyCodesResponse = masterDataServiceImpl.validateCurrencyBasedOnCompanyCode(companyCodes.getId());
        Assert.assertEquals("234445" , companyCodesResponse.getId());

    }
    @Test
    public void testGetCurrencyBasedOnCompanyCodes() {
        CompanyCodes companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setAddress("Australia");
        companyCodes.setId("234445");
        when(masterDataDao.getCurrencyBasedOnCompanyCodes(any(String.class))).thenReturn(companyCodes);
        CompanyCodes companyCodesResponse = masterDataServiceImpl.getCurrencyBasedOnCompanyCodes(companyCodes.getId());
        Assert.assertEquals("234445" , companyCodesResponse.getId());

    }
    @Test
    public void testGetCompanyCodeID() {
        CompanyCodes companyCodes = Struct.create(CompanyCodes.class);
        companyCodes.setAddress("Australia");
        companyCodes.setId("234445");
        when(masterDataDao.getCompanyCodeID(any(String.class))).thenReturn(companyCodes);
        CompanyCodes companyCodesResponse = masterDataServiceImpl.getCompanyCodeID(companyCodes.getId());
        Assert.assertEquals("234445" , companyCodesResponse.getId());

    }

    @Test
    public void testGetAction() {
        Result result = mock(Result.class);
        when(masterDataDao.getAction(any(String.class))).thenReturn(result);
        masterDataServiceImpl.getAction("result");
    }

    @Test
    public void testGetActionPrecondition() {
        Result result = mock(Result.class);
        when(masterDataDao.getActionPrecondition(any(String.class))).thenReturn(result);
        masterDataServiceImpl.getActionPrecondition("result");
    }
    @Test
    public void testGetCompanyCodeBasedOnPlants(){
        Plants plants = Struct.create(Plants.class);
        plants.setAddress("Australia");
        plants.setId("234445");
        when(masterDataDao.getCompanyCodeBasedOnPlants(any(String.class))).thenReturn(plants);
        Plants plantsResponse = masterDataServiceImpl.getCompanyCodeBasedOnPlants(plants.getId());
        Assert.assertEquals("234445" , plantsResponse.getId());
    }

    @Test
    public void testValidateComplanyCodeBasedOnPlant(){
        Plants plants = Struct.create(Plants.class);
        plants.setAddress("Australia");
        plants.setId("234445");
        when(masterDataDao.validateCompanyCodeBasedOnPlant(any(String.class))).thenReturn(plants);
        Plants plantsResponse = masterDataServiceImpl.validateComplanyCodeBasedOnPlant(plants.getId());
        Assert.assertEquals("234445" , plantsResponse.getId());
    }
    @Test
    public void testGetUnitOfMeasure(){
        when(unitOfMeasureRepo.getUnitOfMeasureDetails(any(String.class))).thenReturn(result);
        masterDataServiceImpl.getUnitOfMeasure("jb");

    }
}
