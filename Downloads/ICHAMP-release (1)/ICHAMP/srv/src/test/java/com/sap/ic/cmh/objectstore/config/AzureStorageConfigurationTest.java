package com.sap.ic.cmh.objectstore.config;

import com.sap.ic.cmh.objectstore.model.DestinationConfiguration;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import org.jclouds.blobstore.BlobStoreContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.when;

public class AzureStorageConfigurationTest {
    @InjectMocks
    AzureStorageConfiguration controller;

    @Mock
    ObjectStoreUtil objectStoreUtil;

    @Mock
    DestinationConfiguration destinationConfiguration;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void getBlobStoreContextTest(){
        when(objectStoreUtil.getDestinationConfiguration("subdomain")).thenReturn(destinationConfiguration);
        when(destinationConfiguration.getAccountName()).thenReturn("BTP-USERS");
        when(destinationConfiguration.getSasToken()).thenReturn("Sdfgyhnjsjmm");
        BlobStoreContext subdomain = controller.getBlobStoreContext("subdomain");
        Assert.assertNotNull(subdomain.getBlobStore());
    }

}
