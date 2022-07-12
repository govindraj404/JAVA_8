package com.sap.ic.cmh.masterdata.distributionchannel.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.ic.cmh.masterdata.common.model.CommonRequest;

import java.util.Objects;

public class DistributionChannelRequest extends CommonRequest {
    @JsonProperty("distributionChannel")
    private String distributionChannel;

    @JsonProperty("salesOrganization")
    private String salesOrganization;


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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DistributionChannelRequest that = (DistributionChannelRequest) o;
        return Objects.equals(distributionChannel, that.distributionChannel) &&
                Objects.equals(salesOrganization, that.salesOrganization);
    }

    @Override
    public int hashCode() {
        return Objects.hash(distributionChannel, salesOrganization);
    }
}