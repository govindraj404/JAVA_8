namespace com.sap.ic.cmh.complaintCategory;

using {com.sap.ic.cmh.common.dataType as DataType} from './index';

type ComplaintCategory : Association to one ComplaintCategories;

entity ComplaintCategories {
    key code : DataType.ComplaintCategoryCode;
        name : localized DataType.ComplaintCategoryName;
}
