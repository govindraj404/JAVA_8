namespace com.sap.ic.cmh.customerComplaint.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF noteType.cds ~~~~~~~~~~~~~~~~~~~  */

type NoteTypeCode : String(6);
type NoteTypeDescription : String(30);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF noteType.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF complaintConfirmationStatus.cds ~~~~~~~~~~~~~~~~~~~  */

type ComplaintConfirmationStatusCode : String(4);
type ComplaintConfirmationStatusDescription : String(30);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF complaintConfirmationStatus.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF customerComplaintActionsStatus.cds ~~~~~~~~~~~~~~~~~~~  */

type CustomerComplaintActionsStatusCode : String(4);
type CustomerComplaintActionsDescription : String(30);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF customerComplaintActionsStatus.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF customerComplaintActionsProcessingTypes.cds ~~~~~~~~~~~~~~~~~~~  */

type CustomerComplaintActionsProcessingTypeCode : String(4);
type CustomerComplaintActionsProcessingTypeDescription : String(30);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF customerComplaintActionsProcessingTypes.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF CustomerComplaintActionsExecutable.cds ~~~~~~~~~~~~~~~~~~~  */

type CustomerComplaintActionsExecutableCode : String(1);
type CustomerComplaintActionsExecutableDescription : String(50);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF CustomerComplaintActionsExecutable.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF complaintLifeCycleStatus.cds ~~~~~~~~~~~~~~~~~~~  */

type ComplaintLifeCycleStatusCode : String(4);
type ComplaintLifeCycleStatusDescription : String(20);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF complaintLifeCycleStatus.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF returnFollowUpType.cds ~~~~~~~~~~~~~~~~~~~  */

type ReturnFollowUpTypeCode : String(12);
type ReturnFollowUpTypeDescription : String(40);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF returnFollowUpType.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF referenceDocumentCategory.cds ~~~~~~~~~~~~~~~~~~~  */

type DocumentCategoryCode : String(10);
type DocumentCategoryDescription : String(50);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF referenceDocumentCategory.cds ~~~~~~~~~~~~~~~~~~~~  */


/* ~~~~~~~~~~~~~~~~~~~~~~ START OF note.cds ~~~~~~~~~~~~~~~~~~~  */

type NoteText : String(500);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF note.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF customerComplaintItem.cds ~~~~~~~~~~~~~~~~~~~  */

type PricingProcedure : String(6);
type PricingDate : Date;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF customerComplaintItem.cds ~~~~~~~~~~~~~~~~~~~~  */


/* ~~~~~~~~~~~~~~~~~~~~~~ START OF complaintCategory.cds ~~~~~~~~~~~~~~~~~~~  */

type ComplaintQuantityRuleCode : String(10);
type ComplaintQuantityRuleName : String(100);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF complaintCategory.cds ~~~~~~~~~~~~~~~~~~~~  */


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ START OF Attachment.cds ~~~~~~~~~~~~~~~~~~~~~~  */

type AttachmentName : String(100);
type AttachmentSize : String;
type AttachmentType : String;
type AttachmentURL : String;
type ReadableFileSize : String(10);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Attachment.cds ~~~~~~~~~~~~~~~~~~~~~~~  */

type GrossWeight : Decimal(15, 2);
type NetWeight : Decimal(15, 2);
type WeightUnit : Decimal(15, 2);
type Volume : Decimal(5, 3);
type VolumeUnit : String(3);
type MaterialUnknown : Boolean;
type ComplaintMaterial : String(40);
type ReferenceQuantity : Decimal(15, 2);
type ReceivedQuantity : Decimal(15, 2);
type ReturnQuantity : Decimal(15, 2);
type ComplaintQuantity : Decimal(15, 2);
type ReturnLocation : String(10);
type AdjustmentValue : Decimal(15, 2);
