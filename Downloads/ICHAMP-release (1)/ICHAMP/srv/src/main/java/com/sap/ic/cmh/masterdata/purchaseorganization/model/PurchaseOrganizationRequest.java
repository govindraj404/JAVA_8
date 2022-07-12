package com.sap.ic.cmh.masterdata.purchaseorganization.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class PurchaseOrganizationRequest extends CommonRequest {

    @JsonProperty("purchaseOrganization")
    private String purchaseOrganization;

    public String getPurchaseOrganization() {
        return purchaseOrganization;
    }

    public void setPurchaseOrganization(String purchaseOrganization) {
        this.purchaseOrganization = purchaseOrganization;
    }
}
