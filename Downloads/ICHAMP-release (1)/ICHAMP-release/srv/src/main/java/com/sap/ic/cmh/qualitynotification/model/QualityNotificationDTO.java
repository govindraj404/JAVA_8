package com.sap.ic.cmh.qualitynotification.model;

import java.util.Map;

public class QualityNotificationDTO {

	private SupplierPersonDetails supplierDetails;
	private SupplierPersonDetails personDetails;
    private Map<String,Object> qualityNotifications;


	/**
	 * @return the supplierDetails
	 */
	public SupplierPersonDetails getSupplierDetails() {
		return supplierDetails;
	}

	/**
	 * @param supplierDetails the supplierDetails to set
	 */
	public void setSupplierDetails(SupplierPersonDetails supplierDetails) {
		this.supplierDetails = supplierDetails;
	}

	/**
	 * @return the personDetails
	 */
	public SupplierPersonDetails getPersonDetails() {
		return personDetails;
	}

	/**
	 * @param personDetails the personDetails to set
	 */
	public void setPersonDetails(SupplierPersonDetails personDetails) {
		this.personDetails = personDetails;
	}

	public Map<String, Object> getQualityNotifications() {
		return qualityNotifications;
	}

	public void setQualityNotifications(Map<String, Object> qualityNotifications) {
		this.qualityNotifications = qualityNotifications;
	}

}
