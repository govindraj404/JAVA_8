package com.sap.ic.cmh.claim.model;


import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;


public class ClaimPricing {

    @JsonProperty("refhandle")
    private String refhandle;
    @JsonProperty("currency")
    private String currency;
    @JsonProperty("condType")
    private String condType;
    @JsonProperty("totalCost")
    private String totalCost;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<>();

    @JsonProperty("refhandle")
    public String getRefhandle() {
        return refhandle;
    }

    @JsonProperty("refhandle")
    public void setRefhandle(String refhandle) {
        this.refhandle = refhandle;
    }

    @JsonProperty("currency")
    public String getCurrency() {
        return currency;
    }

    @JsonProperty("currency")
    public void setCurrency(String currency) {
        this.currency = currency;
    }

    @JsonProperty("condType")
    public String getCondType() {
        return condType;
    }

    @JsonProperty("condType")
    public void setCondType(String condType) {
        this.condType = condType;
    }

    @JsonProperty("totalCost")
    public String getTotalCost() {
        return totalCost;
    }

    @JsonProperty("totalCost")
    public void setTotalCost(String totalCost) {
        this.totalCost = totalCost;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}

