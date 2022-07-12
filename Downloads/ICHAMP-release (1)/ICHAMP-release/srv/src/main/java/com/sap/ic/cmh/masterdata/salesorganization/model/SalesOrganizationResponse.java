package com.sap.ic.cmh.masterdata.salesorganization.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class SalesOrganizationResponse extends CommonResponse {

    @JsonProperty("Sales Organization")
    private String salesOrganization;


    @Override
    public String toString() {
        return "SalesOrganizationResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ",salesOrganization='" + salesOrganization + '\'' +
                '}';
    }

    public String getSalesOrganization() {
        return salesOrganization;
    }

    public void setSalesOrganization(String salesOrganization) {
        this.salesOrganization = salesOrganization;
    }

}