package com.sap.ic.cmh.objectstore.util;

/**
 * This enum class stores the different Objectstore plans,providers available in SCP
 *  across landscapes.
 *
 */
public enum CloudProviders {

    PROVIDER_AWS ("aws-s3"),
    PROVIDER_AZURE("azureblob"),

    PROFILE_AWS ("cloud-aws"),
    PROFILE_AZURE("cloud-azure"),

    PLAN_AWS("s3-standard"),
    PLAN_AZURE("azure-standard");

    private final String providerName;

    CloudProviders(final String providerName){
        this.providerName = providerName;
    }

    @Override
    public String toString() {
        return this.providerName;
    }
}
