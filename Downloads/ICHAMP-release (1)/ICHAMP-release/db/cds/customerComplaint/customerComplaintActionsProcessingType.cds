namespace com.sap.ic.cmh.customerComplaintActionsProcessingTypes;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type CustomerComplaintActionProcessingType : Association to one CustomerComplaintActionsProcessingTypes;

entity CustomerComplaintActionsProcessingTypes{
    key code        : DataType.CustomerComplaintActionsProcessingTypeCode;
        description : localized DataType.CustomerComplaintActionsProcessingTypeDescription;
}