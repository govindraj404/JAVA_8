package com.sap.ic.cmh.masterdata.storagelocation.validation;

import cds.gen.masterdataservice.StorageLocations;
import cds.gen.masterdataservice.StorageLocations_;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StorageLocationValidatorImpl implements StorageLocationValidator {

    public static final Logger logger = LoggerHelper.getLogger(StorageLocationValidatorImpl.class);

    @Autowired
    private DataValidator dataValidator;

    /**
     * Method used to validate and sanitize the storage location details
     * @param storageLocation
     */
    @Override
    public void checkInputsSanitized(StorageLocations storageLocation) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "checkInputsSanitized");

        /* Storage Location */
        dataValidator.validateData(storageLocation.getStorageLocation(),
                MessageKeys.STORAGE_LOCATION_VALIDATION_ERROR, StorageLocations_.class, StorageLocations_::storageLocation, true);

        /* Storage Location Name */
        dataValidator.validateData(storageLocation.getStorageLocationName(),
                MessageKeys.STORAGE_LOCATION_NAME_VALIDATION_ERROR, StorageLocations_.class, StorageLocations_::storageLocationName);

        /* Plant Code */
        dataValidator.validateData(storageLocation.getPlantCode(),
                MessageKeys.PLANT_CODE_VALIDATION_ERROR, StorageLocations_.class, StorageLocations_::plantCode);

        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "checkInputsSanitized");
    }
}

