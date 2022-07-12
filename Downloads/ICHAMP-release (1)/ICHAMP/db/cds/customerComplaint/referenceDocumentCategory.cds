namespace com.sap.ic.cmh.referenceDocumentCategory;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';

type ReferenceDocumentCategory : Association to one ReferenceDocumentCategories;

entity ReferenceDocumentCategories {
    key code        : DataType.DocumentCategoryCode;
        description : localized DataType.DocumentCategoryDescription;
}
