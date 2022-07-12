package com.sap.ic.cmh.masterdata.materialmasterplantdata.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class MaterialMasterPlantDataRequest extends CommonRequest {
    @JsonProperty("materialCode")
    private String materialCode;

    @JsonProperty("plant")
    private String plant;

    public String getMaterialCode() {
        return materialCode;
    }

    public void setMaterialCode(String materialCode) {
        this.materialCode = materialCode;
    }

    public String getPlant() {
        return plant;
    }

    public void setPlant(String plant) {
        this.plant = plant;
    }
}