package com.sap.ic.cmh.masterdata.common.persistency;

import com.sap.cds.Result;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.sap.common.Currencies;

public interface MasterDataRepository {
	
	public Result getCurrencies(String currenciesCode);
	
	public Result getCountryCode(String countryKeyCode);
	
	public MaterialMasterGeneralDatas getMaterial(String materialID);
	
	public BusinessPartners getSupplier(String supplierID);
	
	public Plants getPlant(String plantID);
	
	public Plants validateCompanyCodeBasedOnPlant(String companyCode);
	
	public Plants getCompanyCodeBasedOnPlants(String plantId);
	
	public CompanyCodes getCompanyCodeID(String companyCode);
	
	public CompanyCodes getCompanyCode(String companyCodeID);
	
	public PurchaseOrganizations getPurchasingOrganization(String purchasingOrganizationID);
	
	public Currencies getCurrency(String currency);
	
	public CompanyCodes getCurrencyBasedOnCompanyCodes(String companyID);
	
	public CompanyCodes validateCurrencyBasedOnCompanyCode(String currency);
	
	public Result getDefectGroup(String defectCode);
	
	public Result getItemTypeCode(String itemTypeKeyCode);
	
	public Result getAction(String bObject);
	
	public Result getActionPrecondition(String actionCode);
	

}
