namespace com.sap.ic.cmh.qualityNotificationStatus;

type QualityNotificationStatus : Association to one QualityNotificationStatuses;

entity QualityNotificationStatuses {
    key code : String (40);
        name : localized String(60);
        sequenceNumber : Integer;
}