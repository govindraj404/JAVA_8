package com.sap.ic.cmh.masterdata.subitemtype.validation;

import cds.gen.masterdataservice.SubItemTypes;

public interface SubItemTypeValidator {
    void validateSubItemTypeFields(SubItemTypes subItemType);
    void checkInputsSanitized(SubItemTypes subItemType);
    void validateItemTypeCode(String itemTypeCode);
}