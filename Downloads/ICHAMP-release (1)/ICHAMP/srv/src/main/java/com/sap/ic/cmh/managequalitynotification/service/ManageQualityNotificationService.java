package com.sap.ic.cmh.managequalitynotification.service;

import java.util.List;
import com.sap.ic.cmh.configuration.model.MasterData;
import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.managequalitynotificationservice.BusinessObjectStatuses;

public interface ManageQualityNotificationService{
    MasterData getMasterDataIdsBasedOnDetails(QualityNotifications manageQualityNotification);
    List<BusinessObjectStatuses> mapQNStatus(QualityNotifications manageQualityNotification, List<BusinessObjectStatuses> statusListFromBackend);
    QualityNotifications updateQualityNotification(QualityNotifications manageQualityNotification);
    String checkIfActiveQNExistsBasedOnNumber(String qualityNotificationNumber);
    QualityNotifications getQualityNotificationDetailsFromActive(String qnId);

	
}