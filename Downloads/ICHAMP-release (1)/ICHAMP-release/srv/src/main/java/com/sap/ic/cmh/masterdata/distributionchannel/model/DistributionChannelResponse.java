package com.sap.ic.cmh.masterdata.distributionchannel.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonResponse;

public class DistributionChannelResponse extends CommonResponse {

    @JsonProperty("Distribution Channel")
    private String distributionChannel;

    @JsonProperty("salesOrganization")
    private String salesOrganization;

    @Override
    public String toString() {
        return "DistributionChannelResponse{" +
                "status='" + getStatus() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", distributionChannel='" + distributionChannel + '\'' +
                '}';
    }


    public String getDistributionChannel() {
        return distributionChannel;
    }

    public void setDistributionChannel(String distributionChannel) {
        this.distributionChannel = distributionChannel;
    }

    public String getSalesOrganization() {
        return salesOrganization;
    }

    public void setSalesOrganization(String salesOrganization) {
        this.salesOrganization = salesOrganization;
    }
}