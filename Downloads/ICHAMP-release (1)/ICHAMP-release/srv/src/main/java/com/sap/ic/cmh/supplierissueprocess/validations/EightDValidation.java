package com.sap.ic.cmh.supplierissueprocess.validations;

import cds.gen.qualitynotificationservice.QualityNotifications;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses;

public interface EightDValidation {
    void validateEightDFields(Supplier8DProcesses eightD);
    void validateFieldControlEightD(Supplier8DProcesses eightD);
	void validateIf8DExistsForComplaint(String complaintId);
	void validateifBOIsRelevant(String complaintId, String supplierEightdCode);
	void validateIf8DExists(String id);
    void validateQNFields(QualityNotifications qualityNotification);
}
