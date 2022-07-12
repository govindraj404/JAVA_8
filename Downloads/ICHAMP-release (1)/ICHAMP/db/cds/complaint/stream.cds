namespace com.sap.ic.cmh.stream;

using {cuid,managed} from '@sap/cds/common';
using {com.sap.ic.cmh.complaint.Complaint} from './index';
using {com.sap.ic.cmh.businessObject.BusinessObjects} from './index';
using {com.sap.ic.cmh.streamType.StreamType } from './index';
using {com.sap.ic.cmh.streamStatus.StreamStatus } from './index';

type Stream : Association to one Streams;

entity Streams : cuid {
    streamType      : StreamType;
    status          : StreamStatus;
    isRelevant      : Boolean;
    sequenceNumber  : Integer;
    businessObjects : Composition of many BusinessObjects on businessObjects.stream = $self;
    parentID        : Complaint;
}