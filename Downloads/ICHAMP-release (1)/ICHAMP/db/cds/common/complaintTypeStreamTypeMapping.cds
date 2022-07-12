namespace com.sap.ic.cmh.complaintTypeStreamTypeMapping;

using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from './index';
using {com.sap.ic.cmh.streamType.StreamType} from './index';

type ComplaintTypeStreamTypeMapping : Association to one ComplaintTypeStreamTypeMappings;

entity ComplaintTypeStreamTypeMappings {
    key complaintType  : ComplaintCategory;
    key streamType     : StreamType;
        sequenceNumber : Integer;
}
