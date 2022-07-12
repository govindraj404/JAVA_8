package com.sap.ic.cmh.qualitynotification.model;

public class QualityNotificationResponseFromBapi {

	private QualityNotificationDetails qualityNotificationDetails;
	private String errorMessage;

	/**
	 * Get QualityNotificationDetails
	 * 
	 * @return
	 */
	public QualityNotificationDetails getQualityNotificationDetails() {
		return qualityNotificationDetails;
	}

	/**
	 * Set the value for QualityNotificationDetails
	 * 
	 * @param qualityNotificationDetails
	 */
	public void setQualityNotificationDetails(QualityNotificationDetails qualityNotificationDetails) {
		this.qualityNotificationDetails = qualityNotificationDetails;
	}
	
	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

}
