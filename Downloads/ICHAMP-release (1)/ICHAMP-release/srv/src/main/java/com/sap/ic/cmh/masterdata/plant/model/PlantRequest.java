package com.sap.ic.cmh.masterdata.plant.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class PlantRequest extends CommonRequest {

    @JsonProperty("plant")
    private String plant;

    public String getPlant() {
        return plant;
    }

    public void setPlant(String plant) {
        this.plant = plant;
    }
}
