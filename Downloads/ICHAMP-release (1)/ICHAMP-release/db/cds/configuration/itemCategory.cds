namespace com.sap.ic.cmh.itemCategory;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from '../common/index';
using {com.sap.ic.cmh.complaintQuantityRule.ComplaintQuantityRule} from '../customerComplaint/index';
using {com.sap.ic.cmh.referenceDocumentCategory.ReferenceDocumentCategory} from '../customerComplaint/index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';

type ItemCategory : Association to one ItemCategories;

@cds.search    : {
    identifier,
    code,
    description
}
@assert.unique : {code : [code]}

entity ItemCategories : cuid, managed {
    identifier                             : DataType.Identifier;
    code                                   : DataType.Code;
    description                            : DataType.Description;
    complaintCategory                      : ComplaintCategory;
    complaintQuantityRule                  : ComplaintQuantityRule;
    materialEnteredManually                : DataType.Flag;
    receivedQuantityEditable               : DataType.Flag;
    returnQuantityEditable                 : DataType.Flag;
    individualComplaint                    : DataType.Flag;
    externalReferenceMandatory             : DataType.Flag;
    externalReferenceCheckedForDuplication : DataType.Flag;
    relevantForLogisticProcess             : DataType.Flag;
    referenceDocumentCategory              : ReferenceDocumentCategory;
    material                               : MaterialMasterGeneralData;
    creditDebitAmountEnteredManually       : DataType.Flag;
    conditionType                          : DataType.ConditionType_ItemCategory; 
    isActive                               : Boolean default true;
}
