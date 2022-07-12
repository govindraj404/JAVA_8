package com.sap.ic.cmh.objectstore.config;

import com.sap.ic.cmh.objectstore.model.DestinationConfiguration;
import com.sap.ic.cmh.objectstore.util.CloudProviders;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.jclouds.ContextBuilder;
import org.jclouds.blobstore.BlobStoreContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * This is MS Azure Credentials Configuration class
 *
 */

@Service
public class AzureStorageConfiguration {

    private static Logger logger = LoggerFactory.getLogger(AzureStorageConfiguration.class);

    @Autowired
    PlatformUtil platformUtil;
    @Autowired
    ObjectStoreUtil objectStoreUtil;

    public BlobStoreContext getBlobStoreContext(String subdomain) {
        logger.info("Getting the blob store context");
        DestinationConfiguration destinationConfiguration = objectStoreUtil.getDestinationConfiguration(subdomain);
        return ContextBuilder.newBuilder(CloudProviders.PROVIDER_AZURE.toString()).credentials(destinationConfiguration.getAccountName(), destinationConfiguration.getSasToken())
                .buildView(BlobStoreContext.class);
    }

}
