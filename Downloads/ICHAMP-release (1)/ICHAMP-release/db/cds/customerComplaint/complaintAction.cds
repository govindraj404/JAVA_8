namespace com.sap.ic.cmh.complaintAction;

using {
    cuid,
    managed
} from '@sap/cds/common';
using {com.sap.ic.cmh.complaint.dataType as DataType} from './index';
using{com.sap.ic.cmh.customerComplaint.CustomerComplaint} from './index';
using{com.sap.ic.cmh.customerComplaintActionsStatus.CustomerComplaintActionStatus} from './index';
using{com.sap.ic.cmh.customerComplaintActionsProcessingTypes.CustomerComplaintActionProcessingType} from './index';
using{com.sap.ic.cmh.customerComplaintActionsExecutable.CustomerComplaintActionExecutable} from './index';
type ComplaintAction : Association to one ComplaintActions;

entity ComplaintActions : cuid, managed {
    businessObjectType : String(4); // To be adjusted once object type is finalised
    status             : CustomerComplaintActionStatus; // To be adjusted with Predelivered Content
    processingType     : CustomerComplaintActionProcessingType; // To be adjusted with Predelivered Content
    executable         : CustomerComplaintActionExecutable; // To be adjusted with Predelivered Content
    isRelevant         : Boolean;
    businessObjects    : Composition of many ComplaintActionItems
                             on businessObjects.parentID = $self;
    parentID           : CustomerComplaint;
}

entity ComplaintActionItems : cuid, managed {
    identifier : DataType.Identifier;
    parentID   : ComplaintAction;
}
