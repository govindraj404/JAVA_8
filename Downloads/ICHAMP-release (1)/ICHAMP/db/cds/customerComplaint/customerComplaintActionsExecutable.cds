namespace com.sap.ic.cmh.customerComplaintActionsExecutable;
using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type CustomerComplaintActionExecutable: Association to one CustomerComplaintActionsExecutable;

entity CustomerComplaintActionsExecutable{
    key code            : DataType.CustomerComplaintActionsExecutableCode;
        description     : localized DataType.CustomerComplaintActionsExecutableDescription;
}