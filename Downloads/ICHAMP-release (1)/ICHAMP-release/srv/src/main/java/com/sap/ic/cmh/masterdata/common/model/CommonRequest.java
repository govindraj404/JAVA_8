package com.sap.ic.cmh.masterdata.common.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CommonRequest {

    @JsonProperty("recordNo")
    private String recordNo;

    public String getRecordNo() {
        return recordNo;
    }

    public void setRecordNo(String recordNo) {
        this.recordNo = recordNo;
    }
}
