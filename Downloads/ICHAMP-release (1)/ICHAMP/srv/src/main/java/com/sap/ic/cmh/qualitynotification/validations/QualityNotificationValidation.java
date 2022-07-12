package com.sap.ic.cmh.qualitynotification.validations;

import cds.gen.qualitynotificationservice.QualityNotifications;

public interface QualityNotificationValidation {
	
	void validateMandatoryFields(QualityNotifications qualityNotification);
	void validateQNFreeTextFields(QualityNotifications qualityNotification);
	void validateFieldControlQN(QualityNotifications qn);
	void validateIfQNExistsForComplaint(String complaintId);
	void validateIfQNExists(String id);
	void validateIfBOIsRelevant(String complaintId, String qualityNotificationCode);
    void validatePurchaseOrderNumber(String purchaseOrderNumber);
    void validatePurchaseOrderItemNumber(String purchaseOrderItem);
}
