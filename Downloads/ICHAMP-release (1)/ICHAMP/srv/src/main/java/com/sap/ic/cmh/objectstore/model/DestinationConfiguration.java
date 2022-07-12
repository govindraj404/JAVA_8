package com.sap.ic.cmh.objectstore.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DestinationConfiguration{
    @JsonProperty("Name")
    String name;
    @JsonProperty("Type")
    String type;
    @JsonProperty("URL")
    String uRL;
    @JsonProperty("Authentication")
    String authentication;
    @JsonProperty("ProxyType")
    String proxyType;
    String bucket;
    @JsonProperty("account-name")
    String accountName;
    @JsonProperty("secret-access-key")
    String secretAccessKey;
    @JsonProperty("access-key-id")
    String accessKeyId;
    @JsonProperty("sas-token")
    String sasToken;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getuRL() {
        return uRL;
    }

    public void setuRL(String uRL) {
        this.uRL = uRL;
    }

    public String getAuthentication() {
        return authentication;
    }

    public void setAuthentication(String authentication) {
        this.authentication = authentication;
    }

    public String getProxyType() {
        return proxyType;
    }

    public void setProxyType(String proxyType) {
        this.proxyType = proxyType;
    }

    public String getBucket() {
        return bucket;
    }

    public void setBucket(String bucket) {
        this.bucket = bucket;
    }

    public String getAccountName() {
        return accountName;
    }

    public void setAccountName(String accountName) {
        this.accountName = accountName;
    }

    public String getSecretAccessKey() {
        return secretAccessKey;
    }

    public void setSecretAccessKey(String secretAccessKey) {
        this.secretAccessKey = secretAccessKey;
    }

    public String getAccessKeyId() {
        return accessKeyId;
    }

    public void setAccessKeyId(String accessKeyId) {
        this.accessKeyId = accessKeyId;
    }

    public String getSasToken() {
        return sasToken;
    }

    public void setSasToken(String sasToken) {
        this.sasToken = sasToken;
    }
}
