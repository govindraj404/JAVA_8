package com.sap.ic.cmh.masterdata.companycode.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class CompanyCodeResponse extends CommonResponse {

    @JsonProperty("Company Code")
    private String companyCode;

    @Override
    public String toString() {
        return "CompanyCodeResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", companyCode='" + companyCode + '\'' +
                '}';
    }

    public String getCompanyCode() {
        return companyCode;
    }

    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

}
