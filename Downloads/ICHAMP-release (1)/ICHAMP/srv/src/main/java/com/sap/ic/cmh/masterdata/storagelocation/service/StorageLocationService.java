package com.sap.ic.cmh.masterdata.storagelocation.service;

import java.util.List;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationRequest;
import com.sap.ic.cmh.masterdata.storagelocation.model.StorageLocationResponse;
import cds.gen.masterdataservice.StorageLocations;

public interface StorageLocationService {
    List<StorageLocationResponse> deleteStorageLocationList(List<StorageLocationRequest> storageLocationRequest);

    StorageLocations fetchStorageLocations(String storageLocation, String plantId);
}
