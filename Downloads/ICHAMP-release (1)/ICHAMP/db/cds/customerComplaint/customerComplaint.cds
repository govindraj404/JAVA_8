namespace com.sap.ic.cmh.customerComplaint;

using {
    cuid,
    managed,
    Currency
} from '@sap/cds/common';

using {com.sap.ic.cmh.complaintTypeConfiguration.ComplaintTypeConfiguration} from './index';
using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from '../common/index';
using {com.sap.ic.cmh.salesOrganization.SalesOrganization} from '../common/index';
using {com.sap.ic.cmh.distributionChannel.DistributionChannel} from '../common/index';
using {com.sap.ic.cmh.division.Division} from '../common/index';
using {com.sap.ic.cmh.businessPartner.BusinessPartner} from '../common/index';
using {com.sap.ic.cmh.complaintChannel.ComplaintChannel} from '../configuration/index';
using com.sap.ic.cmh.complaint.dataType as ComplaintDataType from '../complaint/index';
using {com.sap.ic.cmh.note.Notes} from './index';
using {com.sap.ic.cmh.attachment.Attachments} from './index';
using {com.sap.ic.cmh.complaintAction.ComplaintActions} from './index';
using {com.sap.ic.cmh.referenceDocumentCategory.ReferenceDocumentCategory} from '../customerComplaint/index';
using {com.sap.ic.cmh.referenceType.ReferenceType} from './index';
using {com.sap.ic.cmh.itemCategory.ItemCategory} from './index';
using {com.sap.ic.cmh.complaintReason.ComplaintReason} from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';
using {com.sap.ic.cmh.returnFollowUpType.ReturnFollowUpType} from './index';
using {com.sap.ic.cmh.complaintLifeCycleStatus.ComplaintLifeCycleStatus} from './index';
using {com.sap.ic.cmh.complaintConfirmationStatus.ComplaintConfirmationStatus} from './index';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasure} from '../common/index';


type CustomerComplaint : Association to one CustomerComplaints;

// @assert.unique : {identifier : [identifier]}
@cds.search : {
    identifier,
    lifeCycleStatus_code,
    lifeCycleStatus.name,
    item.itemCategory.code,
    items.itemCategory.code
}
entity CustomerComplaints : cuid, managed {
    identifier          : ComplaintDataType.Identifier;
    complaintType       : ComplaintTypeConfiguration;
    itemCategory        : ItemCategory;
    complaintCategory   : ComplaintCategory;
    confirmationStatus  : ComplaintConfirmationStatus;
    lifeCycleStatus     : ComplaintLifeCycleStatus;
    soldToParty         : BusinessPartner;
    salesOrganization   : SalesOrganization;
    distributionChannel : DistributionChannel;
    division            : Division;
    externalReference   : ComplaintDataType.Number;
    complaintChannel    : ComplaintChannel;
    currency            : Currency;
    individualComplaint : Boolean;
    adjustmentValue     : DataType.AdjustmentValue;
    complaintReason     : ComplaintReason;
    rejectionReason_ID  : UUID; // Change the field once the configuration application is developed
    item                : Composition of one SingleItems
                              on item.parentID = $self;
    items               : Composition of many MultipleItems
                              on items.parentID = $self;
    notes               : Composition of many Notes
                              on notes.parentID = ID;
    attachments         : Composition of many Attachments
                              on attachments.parentID = ID;
    actions             : Composition of many ComplaintActions
                              on actions.parentID = $self;

    combineItems        : Association to many Items
                              on combineItems.parentID = ID;
}

type SingleItem : Association to one SingleItems;
entity SingleItems : cuid, managed, items {}
type MultipleItem : Association to one MultipleItems;
entity MultipleItems : cuid, managed, items {}

aspect items {
    itemNumber                      : ComplaintDataType.Identifier;
    itemCategory                    : ItemCategory;
    referenceDocumentCategory       : ReferenceDocumentCategory;
    material                        : MaterialMasterGeneralData;
    referenceType                   : ReferenceType;
    referenceDocumentNumber         : ComplaintDataType.Number;
    referenceDocumentLineItemNumber : ComplaintDataType.Number;
    unit                            : UnitOfMeasure;
    grossWeight                     : DataType.GrossWeight;
    netWeight                       : DataType.NetWeight;
    weightUnit                      : DataType.WeightUnit;
    volume                          : DataType.Volume;
    volumeUnit                      : DataType.VolumeUnit;
    materialUnknown                 : DataType.MaterialUnknown;
    complaintMaterial               : DataType.ComplaintMaterial;
    referenceQuantity               : DataType.ReferenceQuantity;
    receivedQuantity                : DataType.ReceivedQuantity;
    returnQuantity                  : DataType.ReturnQuantity;
    complaintQuantity               : DataType.ComplaintQuantity;
    returnFollowupType              : ReturnFollowUpType;
    returnLocation                  : DataType.ReturnLocation;
    confirmationStatus              : ComplaintConfirmationStatus;
    lifeCycleStatus                 : ComplaintLifeCycleStatus;
    rejectionReason_ID              : UUID; // Change the field once the configuration application is developed
    parentID                        : CustomerComplaint;
}

view Items as
        select from SingleItems {
            key ID                        as ID,
                itemNumber                as itemNumber,
                parentID.ID               as parentID,
                itemCategory              as itemCategory,
                referenceDocumentCategory as referenceDocumentCategory,
                material                  as material,
                referenceType             as referenceType,
                referenceDocumentNumber   as referenceDocumentNumber
        }
    union
        select from MultipleItems {
            key ID                        as ID,
                itemNumber                as itemNumber,
                parentID.ID               as parentID,
                itemCategory              as itemCategory,
                referenceDocumentCategory as referenceDocumentCategory,
                material                  as material,
                referenceType             as referenceType,
                referenceDocumentNumber   as referenceDocumentNumber
        };
