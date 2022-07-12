package com.sap.ic.cmh.qualitynotification.model;

import java.util.List;

public class QualityNotificationDetails {

    private String notificationNumber;
	private String notificationType;
	private String material;
	private String plant;
	private String purchasingOrg;
	private String quantity;
	private String unitCode;
	private String supplier;
	private String personResponsible;
	private String purchaseOrderNumber;
	private String purchaseOrderItem;
	private List<SystemStatus> systemStatus;
	private List<QualityNotificaitonItemDetails> itemDetails;
	private String referenceNumber;

	public String getNotificationType() {
		return notificationType;
	}

	public void setNotificationType(String notificationType) {
		this.notificationType = notificationType;
	}

	public String getMaterial() {
		return material;
	}

	public void setMaterial(String material) {
		this.material = material;
	}

	public String getPlant() {
		return plant;
	}

	public void setPlant(String plant) {
		this.plant = plant;
	}

	public String getPurchasingOrg() {
		return purchasingOrg;
	}

	public void setPurchasingOrg(String purchasingOrg) {
		this.purchasingOrg = purchasingOrg;
	}

	public String getQuantity() {
		return quantity;
	}

	public void setQuantity(String quantity) {
		this.quantity = quantity;
	}

	public String getUnitCode() {
		return unitCode;
	}

	public void setUnitCode(String unitCode) {
		this.unitCode = unitCode;
	}

	public String getSupplier() {
		return supplier;
	}

	public void setSupplier(String supplier) {
		this.supplier = supplier;
	}

	public String getPersonResponsible() {
		return personResponsible;
	}

	public void setPersonResponsible(String personResponsible) {
		this.personResponsible = personResponsible;
	}

	public String getPurchaseOrderNumber() {
		return purchaseOrderNumber;
	}

	public void setPurchaseOrderNumber(String purchaseOrderNumber) {
		this.purchaseOrderNumber = purchaseOrderNumber;
	}

	public String getPurchaseOrderItem() {
		return purchaseOrderItem;
	}

	public void setPurchaseOrderItem(String purchaseOrderItem) {
		this.purchaseOrderItem = purchaseOrderItem;
	}

	public List<SystemStatus> getSystemStatus() {
		return systemStatus;
	}

	public void setSystemStatus(List<SystemStatus> systemStatus) {
		this.systemStatus = systemStatus;
	}

	public List<QualityNotificaitonItemDetails> getItemDetails() {
		return itemDetails;
	}

	public void setItemDetails(List<QualityNotificaitonItemDetails> itemDetails) {
		this.itemDetails = itemDetails;
    }
    
    public String getNotificationNumber() {
		return notificationNumber;
	}

	public void setNotificationNumber(String notificationNumber) {
		this.notificationNumber = notificationNumber;
	}
	
	public String getReferenceNumber() {
		return referenceNumber;
	}

	public void setReferenceNumber(String referenceNumber) {
		this.referenceNumber = referenceNumber;
	}

	@Override
	public String toString() {
		return "QualityNotificationDetails [notificationNumber=" + notificationNumber + ", notificationType="
				+ notificationType + ", material=" + material + ", plant=" + plant + ", purchasingOrg=" + purchasingOrg
				+ ", quantity=" + quantity + ", unitCode=" + unitCode + ", supplier=" + supplier
				+ ", personResponsible=" + personResponsible + ", purchaseOrderNumber=" + purchaseOrderNumber
				+ ", purchaseOrderItem=" + purchaseOrderItem + ", systemStatus=" + systemStatus + ", itemDetails="
				+ itemDetails + "]";
	}

}
