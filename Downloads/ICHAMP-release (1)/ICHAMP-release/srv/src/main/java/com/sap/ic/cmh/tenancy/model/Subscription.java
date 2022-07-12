package com.sap.ic.cmh.tenancy.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Subscription {

    private String url;
    private String subdomain;
    private String appName;
    private String consumerTenantId;
    private String globalAccountId;
    private String subaccountId;
    private String state;
    private String createdOn;
    private String changedOn;
    private String internalSubscriptionId;
    
    /**
     * @return the url
     */
    public String getUrl() {
        return url;
    }
   
    /**
     * @return the subdomain
     */
    public String getSubdomain() {
        return subdomain;
    }
    
    /**
     * @return the appName
     */
    public String getAppName() {
        return appName;
    }
    
    /**
     * @return the consumerTenantId
     */
    public String getConsumerTenantId() {
        return consumerTenantId;
    }
    
    /**
     * @return the globalAccountId
     */
    public String getGlobalAccountId() {
        return globalAccountId;
    }
    
    /**
     * @return the subaccountId
     */
    public String getSubaccountId() {
        return subaccountId;
    }
    
    /**
     * @return the state
     */
    public String getState() {
        return state;
    }
    
    /**
     * @return the createdOn
     */
    public String getCreatedOn() {
        return createdOn;
    }
    
    /**
     * @return the changedOn
     */
    public String getChangedOn() {
        return changedOn;
    }
    
    /**
     * @return the internalSubscriptionId
     */
    public String getInternalSubscriptionId() {
        return internalSubscriptionId;
    }

}

