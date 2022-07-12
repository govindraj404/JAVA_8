package com.sap.ic.cmh.masterdata.companycode.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class CompanyCodeRequest extends CommonRequest {

    @JsonProperty("companyCode")
    private String companyCode;

    public String getCompanyCode() {
        return companyCode;
    }

    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

}
