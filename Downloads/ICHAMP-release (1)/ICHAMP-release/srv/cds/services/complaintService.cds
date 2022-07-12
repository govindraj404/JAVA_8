using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.complaint.dataType as datatype} from '../../../db/cds/complaint/index';

service ComplaintService {
  entity Complaints                    as projection on cmh.complaint.Complaints {
    * ,
    @Core.Computed : false 7 as isFieldControlMandatory : Integer,
    @Core.Computed : false 3 as isComplaintNew : Integer,
    @Core.Computed : false false as isShowStreams  : datatype.UpdateRestricted, 
    @Core.Computed : false false as isHideAdaptStreams  : datatype.UpdateRestricted, 
    @Core.Computed : false false as isHideCostCollection : datatype.HideCost,
    @Core.Computed : false false as isUpdateRestricted : datatype.UpdateRestricted,
    @Core.Computed : false false as isEventSpecificRequest  : Boolean,
    false as isHideDiscardComplaint : Boolean,
    false as isHideReopenComplaint  : Boolean,
    false as isHideCloseComplaint  : Boolean,
    businessObjects as draftBusinessObjects
  } actions {
    @(Common.SideEffects : {
            TargetProperties : ['complaintStatus_code', 'isHideReopenComplaint', 'isHideCloseComplaint', 'isHideAdaptStreams', 'isUpdateRestricted'],
            },
            // cds.odata.bindingparameter.name : '_it',
            // Core.OperationAvailable         : _it.isHideCloseComplaint
            Common.IsActionCritical: true
        )
    action Close() returns Complaints;

    @(Common.SideEffects : {
            TargetProperties : ['complaintStatus_code', 'isHideReopenComplaint', 'isHideCloseComplaint', 'isHideAdaptStreams', 'isUpdateRestricted'],
            },
            // cds.odata.bindingparameter.name : '_it',
            // Core.OperationAvailable         : _it.isHideReopenComplaint
            Common.IsActionCritical: true
        )  
    action Reopen() returns Complaints;

    @(Common.SideEffects : {
            TargetProperties : ['complaintStatus_code', 'isHideAdaptStreams', 'isUpdateRestricted', 'isHideDiscardComplaint', 'businessObjects'],
            },
            TargetEntities   : [BusinessObjects],
            // cds.odata.bindingparameter.name : '_it',
            // Core.OperationAvailable         : _it.isHideDiscardComplaint
        Common.IsActionCritical: true
    )
    action Discard() returns Complaints;
  };


  entity BusinessObjects                    as projection on cmh.businessObject.BusinessObjects{
    *,
    @Core.Computed: false '' as isValidActionPrecondition : Boolean,
    @Core.Computed: false '' as isVisible : Boolean
  };

  @readonly
  entity Actions                       as projection on cmh.action.Actions;

  @readonly
  entity ActionPreconditions           as projection on cmh.actionPrecondition.ActionPreconditions;

  @readonly
  entity CombineBusinessObjectStatuses as projection on cmh.combineBusinessObjectStatus.CombineBusinessObjectStatuses;
  
  @readonly
  entity BusinessObjectMappings as projection on cmh.mapBusinessObjects.BusinessObjectMappings;
  @readonly 
  entity ProcessFlow as projection on cmh.processFlow.ProcessFlow;

  entity Streams as projection on cmh.stream.Streams;
  
    @readonly
    entity BTPUsers {
        personResponsibleNumber : String;
        personResponsible_ID : String
     }

    @readonly
    entity Claim as projection on cmh.claim.Claims {  
       *,
      @Core.Computed : false '' as navigation : String(20), 
      '' as number : String(20)
     };


    @readonly
    entity Supplier8DProcess          as projection on cmh.supplierIssueProcess.SupplierIssueProcesses
     {  
       *,
      @Core.Computed : false '' as navigation : String(20), 
      '' as number : String(20)
     };

    @readonly
    entity ReturnPurchaseOrder         as projection on cmh.returnPurchaseOrder.ReturnPurchaseOrders
     {
        *,
      @Core.Computed : false '' as navigation : String(20), 
      '' as number : String(20)
     };

    @readonly
    entity QualityNotification          as projection on cmh.qualityNotification.QualityNotifications {
      *,
      @Core.Computed : false '' as navigation : String(20), 
      '' as number : String(20)
    };
}