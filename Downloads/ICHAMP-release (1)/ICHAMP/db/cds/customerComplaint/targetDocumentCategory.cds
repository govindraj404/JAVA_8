namespace com.sap.ic.cmh.targetDocumentCategory;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type TargetDocumentCategory : Association to one TargetDocumentCategories;

entity TargetDocumentCategories {
    key code        : DataType.DocumentCategoryCode;
        description : localized DataType.DocumentCategoryDescription;
}
