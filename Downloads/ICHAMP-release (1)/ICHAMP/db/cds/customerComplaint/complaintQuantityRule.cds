namespace com.sap.ic.cmh.complaintQuantityRule;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type ComplaintQuantityRule : Association to one ComplaintQuantityRules;

entity ComplaintQuantityRules {
    key code : DataType.ComplaintQuantityRuleCode;
        name : localized DataType.ComplaintQuantityRuleName;
}
