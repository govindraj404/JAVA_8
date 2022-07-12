package com.sap.ic.cmh.qualitynotification.service;

import cds.gen.qualitynotificationservice.Defects;
import cds.gen.qualitynotificationservice.QualityNotifications;

public interface QualityNotificationService {

	void createQualityNotification(QualityNotifications qn);

	void updateQualityNotification(QualityNotifications qn);

	QualityNotifications getQualityNotificationDetails(String qnId);

	void setConfiguredValues(QualityNotifications qn, String boType, String complaintTypeCode);

	QualityNotifications getQualityNotificationDetailsByComplaintId(String complaintId);

	void validateQNDetails(QualityNotifications qn);

	void validateIfQNExistsForComplaint(String complaintId);

	void validateQNExistsAndFieldControl(QualityNotifications qn);

	Defects getDefectBasedOnQN(String qnId);

	void checkIfDuplicateQNExistsAndDelete(String qnNumber);

	QualityNotifications getStatusAndCompanyCode(String qnId);

	QualityNotifications getDraftQualityNotificationByComplaintID(String complaintId);

	Defects getDraftDefectByQualityNotificationID(String qualityNotificationId);

	void deleteDraftQualityNotificationByID(String qnId);

	String checkIfQNExistsBasedOnNumber(String qnNumber);

}
