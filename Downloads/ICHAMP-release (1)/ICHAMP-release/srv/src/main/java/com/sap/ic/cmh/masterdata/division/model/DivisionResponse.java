package com.sap.ic.cmh.masterdata.division.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class DivisionResponse extends CommonResponse {

    @JsonProperty("Sales Division")
    private String salesDivision;

    @JsonProperty("SalesOrganization")
    private String salesOrganization;

    @Override
    public String toString() {
        return "DivisionResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", salesDivision='" + salesDivision + '\'' +
                '}';
    }

    public String getSalesDivision() {
        return salesDivision;
    }

    public void setSalesDivision(String salesDivision) {
        this.salesDivision = salesDivision;
    }

    public String getSalesOrganization() {
        return salesOrganization;
    }

    public void setSalesOrganization(String salesOrganization) {
        this.salesOrganization = salesOrganization;
    }

}