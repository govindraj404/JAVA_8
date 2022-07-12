package com.sap.ic.cmh.masterdata.purchaseorganization.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class PurchaseOrganizationResponse extends CommonResponse {

    @JsonProperty("Purchase Organization")
    private String purchaseOrganization;

    public String getPurchaseOrganization() {
        return purchaseOrganization;
    }

    public void setPurchaseOrganization(String purchaseOrganization) {
        this.purchaseOrganization = purchaseOrganization;
    }
}
