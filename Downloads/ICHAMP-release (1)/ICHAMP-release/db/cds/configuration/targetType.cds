namespace com.sap.ic.cmh.targetType;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.targetDocumentCategory.TargetDocumentCategory} from '../customerComplaint/index';

type TargetType : Association to one TargetTypes;

@cds.search    : {
    identifier,
    code,
    description,
    targetDocumentCategory
}
@assert.unique : {code : [code]}
entity TargetTypes : cuid, managed {
    identifier             : DataType.Identifier;
    code                   : DataType.TargetTypeCode;
    description            : DataType.Description;
    targetDocumentCategory : TargetDocumentCategory;
    isActive               : Boolean default true;
}
