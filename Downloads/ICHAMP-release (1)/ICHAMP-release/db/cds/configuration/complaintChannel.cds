namespace com.sap.ic.cmh.complaintChannel;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';

type ComplaintChannel : Association to one ComplaintChannels;

@cds.search    : {
    identifier,
    code,
    description
}
@assert.unique : {code : [code]}
entity ComplaintChannels : cuid, managed {
    identifier  : DataType.Identifier;
    code        : DataType.Code;
    description : DataType.Description;
    isActive    : Boolean default true;
}
