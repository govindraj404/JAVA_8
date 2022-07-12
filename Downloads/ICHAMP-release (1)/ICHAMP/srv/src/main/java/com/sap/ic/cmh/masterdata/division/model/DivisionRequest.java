package com.sap.ic.cmh.masterdata.division.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

import java.util.Objects;

public class DivisionRequest extends CommonRequest {

    @JsonProperty("salesDivision")
    private String salesDivision;

    @JsonProperty("salesOrganization")
    private String salesOrganization;

    public String getDivision() {
        return salesDivision;
    }

    public void setDivision(String salesDivision) {
        this.salesDivision = salesDivision;
    }

    public String getSalesOrganization() {
        return salesOrganization;
    }

    public void setSalesOrganization(String salesOrganization) {
        this.salesOrganization = salesOrganization;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DivisionRequest that = (DivisionRequest) o;
        return Objects.equals(salesDivision, that.salesDivision) &&
                Objects.equals(salesOrganization, that.salesOrganization);
    }

    @Override
    public int hashCode() {
        return Objects.hash(salesDivision, salesOrganization);
    }
}
