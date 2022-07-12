package com.sap.ic.cmh.configuration.model;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;

public class MasterData {

	private MaterialMasterGeneralDatas material;
	private BusinessPartners supplier;
	private Plants plants;
	private PurchaseOrganizations purchaseOrg;
	private BusinessPartners personResponsible;

	public MaterialMasterGeneralDatas getMaterial() {
		return material;
	}

	public void setMaterial(MaterialMasterGeneralDatas material) {
		this.material = material;
	}

	public BusinessPartners getSupplier() {
		return supplier;
	}

	public void setSupplier(BusinessPartners supplier) {
		this.supplier = supplier;
	}

	public Plants getPlants() {
		return plants;
	}

	public void setPlants(Plants plants) {
		this.plants = plants;
	}

	public PurchaseOrganizations getPurchaseOrg() {
		return purchaseOrg;
	}

	public void setPurchaseOrg(PurchaseOrganizations purchaseOrg) {
		this.purchaseOrg = purchaseOrg;
	}

	public BusinessPartners getPersonResponsible() {
		return personResponsible;
	}

	public void setPersonResponsible(BusinessPartners personResponsible) {
		this.personResponsible = personResponsible;
	}
	
	

}