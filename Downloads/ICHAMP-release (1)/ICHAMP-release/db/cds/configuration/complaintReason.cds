namespace com.sap.ic.cmh.complaintReason;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';

type ComplaintReason : Association to one ComplaintReasons;

@cds.search    : {
    identifier,
    code,
    description
}
@assert.unique : {code : [code]}
entity ComplaintReasons : cuid, managed {
    identifier  : DataType.Identifier;
    code        : DataType.Code;
    description : DataType.Description;
    isActive    : Boolean default true;
}
