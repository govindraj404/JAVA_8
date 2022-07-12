namespace com.sap.ic.cmh.qualityStreamStatus;

using {com.sap.ic.cmh.qualityNotificationStatus.QualityNotificationStatus} from '../qualityNotification/index';
using {com.sap.ic.cmh.supplierIssueProcessStatus.SupplierIssueProcessStatus} from '../supplier8D/index';
using {com.sap.ic.cmh.streamStatus.StreamStatus } from './index';

type QualityStreamStatus : Association to one QualityStreamStatuses;

entity QualityStreamStatuses {
    key sequenceNumber : Integer;
    qualityNotificationStatus : QualityNotificationStatus;
    supplierIssueProcessStatus : SupplierIssueProcessStatus;
    streamStatus : StreamStatus;
}