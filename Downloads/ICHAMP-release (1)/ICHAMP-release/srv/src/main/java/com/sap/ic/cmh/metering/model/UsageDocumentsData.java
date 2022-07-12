package com.sap.ic.cmh.metering.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UsageDocumentsData {
    private List<UsageDocument> usage;

    public List<UsageDocument> getUsage() {
        return usage;
    }

    public void setUsage(List<UsageDocument> usage) {
        this.usage = usage;
    }
}
