package com.sap.ic.cmh.masterdata.materialmasterplantdata.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class MaterialMasterPlantDataResponse extends CommonResponse {

    @JsonProperty("Material Code")
    private String materialCode;

    @Override
    public String toString() {
        return "MaterialMasterGeneralDataResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", materialCode='" + materialCode + '\'' +
                '}';
    }

    public String getMaterialCode() {
        return materialCode;
    }

    public void setMaterialCode(String materialCode) {
        this.materialCode = materialCode;
    }

}