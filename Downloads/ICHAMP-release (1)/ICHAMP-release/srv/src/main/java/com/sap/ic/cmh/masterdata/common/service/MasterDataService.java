package com.sap.ic.cmh.masterdata.common.service;

import com.sap.cds.Result;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.sap.common.Currencies;

public interface MasterDataService {

    public MaterialMasterGeneralDatas getMaterial(String materialID);

    public BusinessPartners getSupplier(String supplierID);

    public Plants getPlant(String plantID);

    public CompanyCodes getCompanyCode(String companyCodeID);

    public Plants validateComplanyCodeBasedOnPlant(String companyCode);

    public PurchaseOrganizations getPurchasingOrganization(String purchasingOrganizationID);

    public Currencies getCurrency(String currency);

    public CompanyCodes validateCurrencyBasedOnCompanyCode(String currency);

    public CompanyCodes getCurrencyBasedOnCompanyCodes(String companyID);

    public Plants getCompanyCodeBasedOnPlants(String plantID);

    public Result getAction(String bObject);

    public Result getActionPrecondition(String actionCode);

    public CompanyCodes getCompanyCodeID(String complanyCode);

    public Result getUnitOfMeasure(String unit);
}
