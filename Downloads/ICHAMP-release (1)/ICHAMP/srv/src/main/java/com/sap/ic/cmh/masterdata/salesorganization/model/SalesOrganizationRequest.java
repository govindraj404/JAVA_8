package com.sap.ic.cmh.masterdata.salesorganization.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class SalesOrganizationRequest extends CommonRequest {

    @JsonProperty("salesOrganization")
    private String salesOrganization;


    public String getSalesOrganization() {
        return salesOrganization;
    }

    public void setSalesOrganization(String salesOrganization) {
        this.salesOrganization = salesOrganization;
    }

}