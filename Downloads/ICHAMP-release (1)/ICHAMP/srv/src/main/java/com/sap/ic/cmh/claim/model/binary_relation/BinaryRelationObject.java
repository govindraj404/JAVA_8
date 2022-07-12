package com.sap.ic.cmh.claim.model.binary_relation;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BinaryRelationObject {
    @JsonProperty("object_key")
    private String objectKey;
    @JsonProperty("object_type")
    private String objectType;
    @JsonProperty("log_sys")
    private String logSys;

    public String getObjectKey() {
        return objectKey;
    }

    public void setObjectKey(String objectKey) {
        this.objectKey = objectKey;
    }

    public String getObjectType() {
        return objectType;
    }

    public void setObjectType(String objectType) {
        this.objectType = objectType;
    }

    public String getLogSys() {
        return logSys;
    }

    public void setLogSys(String logSys) {
        this.logSys = logSys;
    }
}
 