package com.sap.ic.cmh.configuration.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;

import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.CompanyCodes_;

import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;

@Repository
public class ConfigurationDao {

	@Autowired
	PersistenceService db;

	/**
	 * DB call to fetch Material Data based on its ID
	 * 
	 * @param materialId
	 * @return
	 */
	public Result getMaterialData(String materialId) {
		CqnSelect select = Select.from(MaterialMasterGeneralDatas_.class).columns(MaterialMasterGeneralDatas.ID,
            MaterialMasterGeneralDatas.MATERIAL_CODE)
				.where(b -> b.get(MaterialMasterGeneralDatas.ID).eq(materialId));
		return db.run(select);
	}

	/**
	 * DB call to fetch Plant Data based on its ID
	 * 
	 * @param plantId
	 * @return
	 */
	public Result getPlantData(String plantId) {
		CqnSelect select = Select.from(Plants_.class).columns(Plants.ID,Plants.PLANT)
        .where(b -> b.get(Plants.ID).eq(plantId));
		return db.run(select);
	}

	/**
	 * DB call to fetch BusinessPartners Data based on its ID
	 * 
	 * @param supplierId
	 * @return
	 */
	public Result getSupplierData(String supplierId) {
		CqnSelect select = Select.from(BusinessPartners_.class).columns(BusinessPartners.ID,BusinessPartners.BUSINESS_PARTNER_NUMBER
        ,BusinessPartners.IS_MARKED_FOR_DELETION,BusinessPartners.BUSINESS_PARTNER_TYPE)
        .where(b -> b.get(BusinessPartners.ID).eq(supplierId));
		return db.run(select);
	}

	/**
	 * DB call to fetch PurchaseOrganizations Data based on its ID
	 * 
	 * @param purchaseOrgId
	 * @return
	 */
	public Result getPurchOrgData(String purchaseOrgId) {
		CqnSelect select = Select.from(PurchaseOrganizations_.class).columns(PurchaseOrganizations.ID,PurchaseOrganizations.PURCHASE_ORGANIZATION)
				.where(b -> b.get(PurchaseOrganizations.ID).eq(purchaseOrgId));
		return db.run(select);
	}
	/**
	 * DB call to fetch Company code Data based on its ID 
	 * @param companyCodeId
	 * @return
	 */
	public Result getCompanyCodes(String companyCodeId) {
		CqnSelect select = Select.from(CompanyCodes_.class).columns(CompanyCodes.ID,CompanyCodes.COMPANY_CODE)
				.where(b -> b.get(CompanyCodes.ID).eq(companyCodeId));
		return db.run(select);
	}
    
    /**
     * Get Material Data based on Material Code
     * @param materialId
     * @return
     */
    public Result getMaterialDataBasedOnCode(String materialCode) {
		CqnSelect select = Select.from(MaterialMasterGeneralDatas_.class)
				.where(b -> b.get(MaterialMasterGeneralDatas.MATERIAL_CODE).eq(materialCode));
		return db.run(select);
	}
    
    /**
	 * DB call to fetch Plant Data based on the plant
	 * 
	 * @param plantId
	 * @return
	 */
	public Result getPlantDataBasedOnPlant(String plant) {
		CqnSelect select = Select.from(Plants_.class).where(b -> b.get(Plants.PLANT).eq(plant));
		return db.run(select);
	}

	/**
	 * DB call to fetch BusinessPartners Data based on businessPartnerNumber
	 * 
	 * @param supplierId
	 * @return
	 */
	public Result getSupplierDataBasedOnNumber(String supplierNumber) {
		CqnSelect select = Select.from(BusinessPartners_.class).where(b -> b.get(BusinessPartners.BUSINESS_PARTNER_NUMBER).eq(supplierNumber));
		return db.run(select);
	}
	
	/**
	 * DB call to fetch Person Responsible Data based on businessPartnerNumber
	 * 
	 * @param supplierId
	 * @return
	 */
	public Result getPersonResponsibleBasedOnNumber(String personResponsible) {
		CqnSelect select = Select.from(BusinessPartners_.class).where(b -> b.get(BusinessPartners.BUSINESS_PARTNER_NUMBER).eq(personResponsible));
		return db.run(select);
	}

	/**
	 * DB call to fetch PurchaseOrganizations Data based on Purchase org 
	 * 
	 * @param purchaseOrgId
	 * @return
	 */
	public Result getPurchOrgDataBasedOnPurchOrg(String purchaseOrg) {
		CqnSelect select = Select.from(PurchaseOrganizations_.class)
				.where(b -> b.get(PurchaseOrganizations.PURCHASE_ORGANIZATION).eq(purchaseOrg));
		return db.run(select);
	}

}