package com.sap.ic.cmh.masterdata.storagelocation.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class StorageLocationResponse extends CommonResponse {

    @JsonProperty("Plant Code")
    private String plantCode;

    @JsonProperty("Storage Location")
    private String storageLocation;


    public String getPlantCode() {
        return plantCode;
    }

    public void setPlantCode(String plantCode) {
        this.plantCode = plantCode;
    }

    public String getStorageLocation() {
        return storageLocation;
    }

    public void setStorageLocation(String storageLocation) {
        this.storageLocation = storageLocation;
    }

}
