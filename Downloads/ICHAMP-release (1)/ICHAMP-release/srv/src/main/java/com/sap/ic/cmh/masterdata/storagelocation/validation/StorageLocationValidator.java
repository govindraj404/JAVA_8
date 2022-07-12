package com.sap.ic.cmh.masterdata.storagelocation.validation;

import cds.gen.masterdataservice.StorageLocations;

public interface StorageLocationValidator {

    void checkInputsSanitized(StorageLocations storageLocation);
}
