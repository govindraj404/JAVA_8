namespace com.sap.ic.cmh.qualityNotificationStatusMapping;

using {com.sap.ic.cmh.qualityNotificationStatus.QualityNotificationStatus} from './index';

type QualityNotificationStatusMapping : Association to one QualityNotificationStatusMappings;

entity QualityNotificationStatusMappings {
    key code : String (40);
        name : String(40);
        status : QualityNotificationStatus;
}