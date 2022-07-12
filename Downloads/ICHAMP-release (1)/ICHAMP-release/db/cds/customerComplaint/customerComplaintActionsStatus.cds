namespace com.sap.ic.cmh.customerComplaintActionsStatus;
using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type CustomerComplaintActionStatus: Association to one CustomerComplaintActionsStatus;

entity CustomerComplaintActionsStatus{
    key code            : DataType.CustomerComplaintActionsStatusCode;
        description     : localized DataType.CustomerComplaintActionsDescription;
}