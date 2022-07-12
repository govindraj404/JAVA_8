package com.sap.ic.cmh.masterdata.storagelocation.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

import java.util.Objects;

public class StorageLocationRequest extends CommonRequest {

    @JsonProperty("storageLocation")
    private String storageLocation;

    @JsonProperty("plantCode")
    private String plantCode;


    public String getStorageLocation() {
        return storageLocation;
    }

    public void setStorageLocation(String storageLocation) {
        this.storageLocation = storageLocation;
    }

    public String getPlantCode() {
        return plantCode;
    }

    public void setPlantCode(String plantCode) {
        this.plantCode = plantCode;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        StorageLocationRequest that = (StorageLocationRequest) o;
        return Objects.equals(storageLocation, that.storageLocation) &&
                Objects.equals(plantCode, that.plantCode);
    }

    @Override
    public int hashCode() {
        return Objects.hash(storageLocation, plantCode);
    }
}
