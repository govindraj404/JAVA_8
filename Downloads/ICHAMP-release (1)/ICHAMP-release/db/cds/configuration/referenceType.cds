namespace com.sap.ic.cmh.referenceType;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.referenceDocumentCategory.ReferenceDocumentCategory} from '../customerComplaint/index';

type ReferenceType : Association to one ReferenceTypes;

@cds.search    : {
    identifier,
    code,
    description,
    referenceDocumentCategory
}
@assert.unique : {code : [code]}
entity ReferenceTypes : cuid, managed {
    identifier                : DataType.Identifier;
    code                      : DataType.ReferenceTypeCode;
    description               : DataType.Description;
    referenceDocumentCategory : ReferenceDocumentCategory;
    isActive                  : Boolean default true;
}
