package com.sap.ic.cmh.masterdata.materialmastergeneraldata.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class MaterialMasterGeneralDataRequest extends CommonRequest {

    @JsonProperty("materialCode")
    private String materialCode;

    public String getMaterialCode() {
        return materialCode;
    }

    public void setMaterialCode(String materialCode) {
        this.materialCode = materialCode;
    }

}