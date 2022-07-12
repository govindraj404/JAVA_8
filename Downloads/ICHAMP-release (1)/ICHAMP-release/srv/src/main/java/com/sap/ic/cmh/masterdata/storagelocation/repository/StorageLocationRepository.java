package com.sap.ic.cmh.masterdata.storagelocation.repository;

import java.util.List;
import java.util.Map;
import com.sap.cds.Result;

public interface StorageLocationRepository {
    Map<String, String> getStorageLocationMap(List<String> storageLocationList, List<String> plantCodeList);

    void deleteStorageLocationList(List<String> storageLocationList);

    Result fetchStorageLocations(String storageLocation, String plantId);
}
