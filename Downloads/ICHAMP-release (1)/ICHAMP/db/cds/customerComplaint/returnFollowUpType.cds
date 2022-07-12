namespace com.sap.ic.cmh.returnFollowUpType;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type ReturnFollowUpType : Association to one ReturnFollowUpTypes;

entity ReturnFollowUpTypes {
    key code        : DataType.ReturnFollowUpTypeCode;
        description : localized DataType.ReturnFollowUpTypeDescription;
}
