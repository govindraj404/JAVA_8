package com.sap.ic.cmh.masterdata.businesspartner.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

public class BusinessPartnerRequest extends CommonRequest {
    @JsonProperty("businessPartnerNumber")
    private String businessPartner;

    public String getBusinessPartner() {
        return businessPartner;
    }

    public void setBusinessPartner(String businessPartner) {
        this.businessPartner = businessPartner;
    }

}
