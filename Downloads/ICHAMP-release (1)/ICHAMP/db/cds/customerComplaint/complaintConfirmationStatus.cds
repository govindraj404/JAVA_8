namespace com.sap.ic.cmh.complaintConfirmationStatus;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type ComplaintConfirmationStatus : Association to one ComplaintConfirmationStatuses;

entity ComplaintConfirmationStatuses {
    key code        : DataType.ComplaintConfirmationStatusCode;
        description : localized DataType.ComplaintConfirmationStatusDescription;
}
