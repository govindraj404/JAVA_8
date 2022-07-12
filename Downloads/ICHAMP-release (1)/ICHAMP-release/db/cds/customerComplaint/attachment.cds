namespace com.sap.ic.cmh.attachment;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type Attachment : Association to one Attachments;

entity Attachments : managed {
    key ID   : UUID;
    name     : DataType.AttachmentName;
    size     : DataType.AttachmentSize;
    type     : DataType.AttachmentType;
    parentID : UUID;
}
