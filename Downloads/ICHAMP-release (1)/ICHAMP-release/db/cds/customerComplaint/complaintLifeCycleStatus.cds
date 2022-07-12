namespace com.sap.ic.cmh.complaintLifeCycleStatus;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type ComplaintLifeCycleStatus : Association to one ComplaintLifeCycleStatuses;

entity ComplaintLifeCycleStatuses {
    key code        : DataType.ComplaintLifeCycleStatusCode;
        description : localized DataType.ComplaintLifeCycleStatusDescription;
}
