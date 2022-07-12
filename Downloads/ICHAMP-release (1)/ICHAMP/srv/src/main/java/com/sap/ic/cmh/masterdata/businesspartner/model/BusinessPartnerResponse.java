package com.sap.ic.cmh.masterdata.businesspartner.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class BusinessPartnerResponse extends CommonResponse {

    @JsonProperty("BusinessPartner Number")
    private String businessPartner;

    public String getBusinessPartner() {
        return businessPartner;
    }

    public void setBusinessPartner(String businessPartner) {
        this.businessPartner = businessPartner;
    }

}
