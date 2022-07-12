namespace com.sap.ic.cmh.combineBusinessObjectStatus;

using {com.sap.ic.cmh.claimStatus.ClaimStatuses} from '../claim/index';
using {com.sap.ic.cmh.qualityNotificationStatus.QualityNotificationStatuses} from '../qualityNotification/index';
using {com.sap.ic.cmh.returnPurchaseOrderStatus.ReturnPurchaseOrderStatuses} from '../returnPurchaseOrder/index';
using {com.sap.ic.cmh.supplierIssueProcessStatus.SupplierIssueProcessStatuses} from '../supplier8D/index';
using {com.sap.ic.cmh.combineBusinessObject.CommonBusinessObjects} from './index';
using {com.sap.ic.cmh.actionPrecondition.ActionPreconditions} from './index';
type CombineBusinessObjectStatus : Association to one CombineBusinessObjectStatuses;

view CombineBusinessObjectStatuses as
        select from ClaimStatuses {
            key code as code,
            key 'CLM' as businessObjectType : String,
                   name as name,
                   sequenceNumber,
                   'CLP' as streamTypeCode  : String(5),
                    1 as BOSequenceNumber : Integer
        }
    union
        select from QualityNotificationStatuses {
            key code as code,
            key 'QN' as businessObjectType : String,
                name as name,
                sequenceNumber,
                 'QLTY' as streamTypeCode  : String(5),
                 1 as BOSequenceNumber : Integer
        }
    union
        select from ReturnPurchaseOrderStatuses {
            key code as code,
            key 'RPO' as businessObjectType : String,
                name as name,
                sequenceNumber,
                 'LOG' as streamTypeCode  : String(5),
                  1 as BOSequenceNumber : Integer
        }
    union
        select from SupplierIssueProcessStatuses {
            key code as code,
            key 'S8D' as businessObjectType : String,
                name as name,
                sequenceNumber,
                 'QLTY' as streamTypeCode  : String(5),
                  2 as BOSequenceNumber : Integer
        };