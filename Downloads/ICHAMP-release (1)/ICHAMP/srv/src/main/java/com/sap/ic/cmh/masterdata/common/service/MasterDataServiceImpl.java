package com.sap.ic.cmh.masterdata.common.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sap.cds.Result;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.masterdata.unitofmeasure.persistency.UnitOfMeasureRepository;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.sap.common.Currencies;

@Service
public class MasterDataServiceImpl implements MasterDataService {

    @Autowired
    MasterDataDao masterDataDao;

    @Autowired
    UnitOfMeasureRepository unitOfMeasureRepo;

    /**
     * Validate Material
     * 
     * @param materialID
     * @return MaterialMasterGeneralDatas
     */
    @Override
    public MaterialMasterGeneralDatas getMaterial(String materialID) {
        return masterDataDao.getMaterial(materialID);
    }

    /**
     * Validate Supplier
     * 
     * @param supplierID
     * @return BusinessPartners
     */
    @Override
    public BusinessPartners getSupplier(String supplierID) {
        return masterDataDao.getSupplier(supplierID);
    }

    /**
     * Validate Plant
     * 
     * @param plantID
     * @return Plants
     */
    @Override
    public Plants getPlant(String plantID) {
        return masterDataDao.getPlant(plantID);
    }

    /**
     * Validate Company Code
     * 
     * @param companyCodeID
     * @return CompanyCodes
     */
    @Override
    public CompanyCodes getCompanyCode(String companyCodeID) {
        return masterDataDao.getCompanyCode(companyCodeID);
    }

    /**
     * Validate CompanyCode Based on Plant
     * 
     * @param companyCode
     * @return Plants
     */
    @Override
    public Plants validateComplanyCodeBasedOnPlant(String companyCode) {
        return masterDataDao.validateCompanyCodeBasedOnPlant(companyCode);
    }

    /**
     * Validate Purchasingorganization
     * 
     * @param purchasingOrganizationID
     * @return PurchaseOrganizations
     */
    @Override
    public PurchaseOrganizations getPurchasingOrganization(String purchasingOrganizationID) {
        return masterDataDao.getPurchasingOrganization(purchasingOrganizationID);
    }

    /**
     * Validate Currency
     * 
     * @param currency
     * @return Currencies
     */
    @Override
    public Currencies getCurrency(String currency) {
        return masterDataDao.getCurrency(currency);
    }

    /**
     * validate Currency based on Company
     * 
     * @param currency
     * @return CompanyCodes
     */
    @Override
    public CompanyCodes validateCurrencyBasedOnCompanyCode(String currency) {
        return masterDataDao.validateCurrencyBasedOnCompanyCode(currency);

    }

    /**
     * Get Action Based on BusinessObject Type
     * 
     * @param bObject
     * @return Result
     */
    @Override
    public Result getAction(String bObject) {
        return masterDataDao.getAction(bObject);
    }

    /**
     * get ActionPrecondition based on Action Code
     * 
     * @param actionCode
     * @return Result
     */
    @Override
    public Result getActionPrecondition(String actionCode) {
        return masterDataDao.getActionPrecondition(actionCode);
    }

    /**
     * Get currecny based on Company
     * 
     * @param companyID
     * @return CompanyCodes
     */
    @Override
    public CompanyCodes getCurrencyBasedOnCompanyCodes(String companyID) {
        return masterDataDao.getCurrencyBasedOnCompanyCodes(companyID);
    }

    /**
     * get plant based on plant id
     * 
     * @param plantID
     * @return Plants
     */
    @Override
    public Plants getCompanyCodeBasedOnPlants(String plantID) {
        return masterDataDao.getCompanyCodeBasedOnPlants(plantID);
    }

    /**
     * GetCompany based on code
     * 
     * @param companycode
     * @return CompanyCodes
     */
    @Override
    public CompanyCodes getCompanyCodeID(String companycode) {
        return masterDataDao.getCompanyCodeID(companycode);
    }

    /**
     * GetUnit based on code
     * 
     * @param unit
     * @return UnitOfMeasures
     */
    @Override
    public Result getUnitOfMeasure(String unit) {
        return unitOfMeasureRepo.getUnitOfMeasureDetails(unit);
    }

}
