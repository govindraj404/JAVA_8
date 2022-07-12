package com.sap.ic.cmh.metering.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Consumer {
    private String environment;
    private String region;
    private String identityZone;
    private String subAccount;

    public String getEnvironment() {
        return environment;
    }

    public void setEnvironment(String environment) {
        this.environment = environment;
    }

    public String getRegion() {
        return region;
    }

    public void setRegion(String region) {
        this.region = region;
    }

    public String getIdentityZone() {
        return identityZone;
    }

    public void setIdentityZone(String identityZone) {
        this.identityZone = identityZone;
    }

    public String getSubAccount() {
        return subAccount;
    }

    public void setSubAccount(String subAccount) {
        this.subAccount = subAccount;
    }
}
