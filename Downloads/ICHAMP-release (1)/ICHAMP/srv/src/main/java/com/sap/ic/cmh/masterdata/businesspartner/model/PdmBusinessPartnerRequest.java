package com.sap.ic.cmh.masterdata.businesspartner.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PdmBusinessPartnerRequest extends CommonRequest {

    @JsonProperty("dataSubjectId")
    private String businessPartner;

    public String getBusinessPartner() {
        return businessPartner;
    }

    public void setBusinessPartner(String businessPartner) {
        this.businessPartner = businessPartner;
    }
}
