package com.sap.ic.cmh.masterdata.common.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CommonResponse {

    @JsonProperty("Status")
    private String status;

    @JsonProperty("Message")
    private String message;

    @JsonProperty("RecordNo")
    private String recordNo;

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getRecordNo() {
        return recordNo;
    }

    public void setRecordNo(String recordNo) {
        this.recordNo = recordNo;
    }
}
