package com.sap.ic.cmh.masterdata.plant.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class PlantResponse extends CommonResponse {

    @JsonProperty("Plant")
    private String plant;

    public String getPlant() {
        return plant;
    }

    public void setPlant(String plant) {
        this.plant = plant;
    }
}
