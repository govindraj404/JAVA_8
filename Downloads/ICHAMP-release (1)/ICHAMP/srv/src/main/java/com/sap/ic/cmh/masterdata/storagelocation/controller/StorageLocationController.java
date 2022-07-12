package com.sap.ic.cmh.masterdata.storagelocation.controller;

import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationResponse;
import com.sap.ic.cmh.masterdata.storagelocation.service.StorageLocationService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.List;

@RestController
@RequestMapping ("/api/MasterDataService")
public class StorageLocationController {

    public static final Logger logger = LoggerHelper.getLogger(StorageLocationController.class);

    @Autowired
    private StorageLocationService storageLocationService;

    /**
     * Method takes Storage location request as an input to delete
     *
     * @param storageLocationRequest
     * @return List of storage location responses which are nor deleted
     */
    @DeleteMapping ("/StorageLocations")
    public ResponseEntity<List<StorageLocationResponse>> deleteStorageLocation(@RequestBody List<StorageLocationRequest> storageLocationRequest) {
        LoggerHelper.logMethodEntry(logger, "StorageLocationController", "deleteStorageLocation");
        final List<StorageLocationResponse> storageLocationResponseList = storageLocationService.
                deleteStorageLocationList(storageLocationRequest);
        LoggerHelper.logMethodExit(logger, "StorageLocationController", "deleteStorageLocation");
        return new ResponseEntity<>(storageLocationResponseList, HttpStatus.ACCEPTED);
    }
}